import { createHash, randomBytes } from 'node:crypto';
import { mkdir, readFile, open, rename, chmod, unlink } from 'node:fs/promises';
import { dirname } from 'node:path';
import { z } from 'zod';
import type { Reader } from './server.js';
import { subscribeSchema, unsubscribeSchema, type Subscribe, type Unsubscribe } from './event-schema.js';
import { callbackUrl, signingSecret, verifyCallback, signedHeaders, securePost, type WebhookPost } from './webhooks.js';

export class EventError extends Error {
  constructor(public code: number, message: string, public reason: string) { super(message); }
}
const hash = (value: string): string => createHash('sha256').update(value).digest('hex');
export function subscriptionId(owner: string, input: Unsubscribe): string {
  return 'sub_' + hash(JSON.stringify([owner, input.delivery.url, input.name, input.arguments.space]));
}
const messageSchema = z.object({ name: z.string().regex(/^spaces\/[A-Za-z0-9_-]+\/messages\/[A-Za-z0-9_.-]+$/), createTime: z.string(), text: z.string().optional(), thread: z.object({ name: z.string() }).optional() }).passthrough();
type Message = z.infer<typeof messageSchema>;
type Event = { eventId: string; name: 'message.created'; timestamp: string; data: Record<string, string>; cursor: null };
type Pending = { event: Event; attempts: number; nextAttempt: number; failed?: boolean };
type Subscription = {
  id: string; owner: string; space: string; url: string; secret: string;
  previousSecret?: string; rotateUntil?: number; expires: number;
  watermark: number; boundaryIds: string[]; pending: Pending[]; lastError?: string;
};
type State = { version: 1; owner: string; subscriptions: Record<string, Subscription> };
export type Status = { subscriptions: { id: string; space: string; expires: string; queued: number; failed: number; lastError?: string }[] };

// A single background worker owns this file. All operations are serialized;
// replacement is atomic and synced before a checkpoint or success is reported.
export class EventService {
  private state: State;
  private gate: Promise<unknown> = Promise.resolve();
  private verified = new Map<string, number>();
  constructor(private file: string, private owner: string, private read: Reader,
    private post: WebhookPost = securePost, private now: () => number = Date.now) {
    this.state = { version: 1, owner, subscriptions: {} };
  }
  async load(): Promise<void> {
    await mkdir(dirname(this.file), { recursive: true, mode: 0o700 });
    await chmod(dirname(this.file), 0o700);
    try {
      const input = JSON.parse(await readFile(this.file, 'utf8')) as State;
      if (input.version !== 1 || input.owner !== this.owner || !input.subscriptions || typeof input.subscriptions !== 'object') throw new Error('Invalid event state');
      // Fail closed on malformed records rather than replacing lost state.
      for (const sub of Object.values(input.subscriptions)) {
        if (sub.owner !== this.owner || sub.id !== subscriptionId(this.owner, { name: 'message.created', arguments: { space: sub.space }, delivery: { mode: 'webhook', url: sub.url } }) ||
            !Number.isFinite(sub.expires) || !Number.isFinite(sub.watermark) || !Array.isArray(sub.boundaryIds) || !Array.isArray(sub.pending)) throw new Error('Invalid subscription state');
        callbackUrl(sub.url); signingSecret(sub.secret);
      }
      this.state = input; await chmod(this.file, 0o600);
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw new Error('Cannot load private event state');
    }
  }
  private serial<T>(operation: () => Promise<T>): Promise<T> {
    const next = this.gate.then(operation);
    this.gate = next.catch(() => {});
    return next;
  }
  private async save(): Promise<void> {
    const tmp = `${this.file}.${randomBytes(8).toString('hex')}.tmp`;
    const handle = await open(tmp, 'wx', 0o600);
    try { await handle.writeFile(JSON.stringify(this.state)); await handle.sync(); }
    finally { await handle.close(); }
    try {
      await rename(tmp, this.file);
      const dir = await open(dirname(this.file), 'r');
      try { await dir.sync(); } finally { await dir.close(); }
    } catch (error) { await unlink(tmp).catch(() => {}); throw error; }
  }
  subscribe(raw: unknown): Promise<{ id: string; refreshBefore: string; cursor: null; truncated: false }> {
    return this.serial(async () => {
      const input: Subscribe = subscribeSchema.parse(raw);
      try { callbackUrl(input.delivery.url); signingSecret(input.delivery.secret); }
      catch { throw new EventError(-32602, 'Invalid callback URL or signing key', 'invalid_callback'); }
      const id = subscriptionId(this.owner, input);
      const existing = this.state.subscriptions[id];
      if (!existing && Object.keys(this.state.subscriptions).length >= 100) throw new EventError(-32602, 'Subscription limit reached', 'subscription_limit');
      const started = this.now();
      // Haskell refreshes OAuth and checks the verified account and read scope.
      await this.authorize(input.arguments.space);
      const verifiedKey = hash(JSON.stringify([this.owner, input.delivery.url, input.delivery.secret]));
      if ((this.verified.get(verifiedKey) ?? 0) < this.now()) {
        try { await verifyCallback(input.delivery.url, input.delivery.secret, id, this.post); }
        catch { throw new EventError(-32015, 'Callback verification failed', 'challenge_failed'); }
        this.verified.set(verifiedKey, this.now() + 5 * 60_000);
        if (this.verified.size > 500) this.verified.delete(this.verified.keys().next().value!);
      }
      const expires = this.now() + Math.max(60_000, Math.min(input.ttlMs ?? 24 * 60 * 60_000, 7 * 24 * 60 * 60_000));
      const sub: Subscription = existing && existing.expires > this.now() ? { ...existing, expires } : {
        id, owner: this.owner, space: input.arguments.space, url: input.delivery.url,
        secret: input.delivery.secret, expires, watermark: started, boundaryIds: [], pending: [],
      };
      if (sub.secret !== input.delivery.secret) {
        sub.previousSecret = sub.secret; sub.rotateUntil = this.now() + 5 * 60_000; sub.secret = input.delivery.secret;
      }
      // An explicit refresh can recover terminal deliveries after the owner
      // repairs the destination. Retry the same IDs, never invent new events.
      sub.pending = sub.pending.map(item => item.failed ? { ...item, failed: false, attempts: 0, nextAttempt: 0 } : item);
      this.state.subscriptions[id] = sub;
      await this.save();
      return { id, refreshBefore: new Date(expires).toISOString(), cursor: null, truncated: false };
    });
  }
  unsubscribe(raw: unknown): Promise<Record<string, never>> {
    return this.serial(async () => {
      const input = unsubscribeSchema.parse(raw);
      delete this.state.subscriptions[subscriptionId(this.owner, input)];
      await this.save(); return {};
    });
  }
  status(): Promise<Status> {
    return this.serial(async () => ({ subscriptions: Object.values(this.state.subscriptions).map(sub => ({
      id: sub.id, space: sub.space, expires: new Date(sub.expires).toISOString(),
      queued: sub.pending.filter(p => !p.failed).length, failed: sub.pending.filter(p => p.failed).length,
      ...(sub.lastError ? { lastError: sub.lastError } : {}),
    })) }));
  }
  private async authorize(space: string): Promise<void> {
    const reply = await this.read({ action: 'messages', space, limit: 1 });
    if (reply.account !== this.owner) throw new EventError(-32602, 'Google account mismatch', 'account_mismatch');
  }
  tick(): Promise<void> {
    return this.serial(async () => {
      for (const sub of Object.values(this.state.subscriptions)) {
        if (sub.expires <= this.now()) { delete this.state.subscriptions[sub.id]; continue; }
        try {
          // A successful read rechecks authorization before queued deliveries.
          await this.collect(sub);
          delete sub.lastError;
          if ((sub.rotateUntil ?? 0) <= this.now()) { delete sub.previousSecret; delete sub.rotateUntil; }
          await this.deliver(sub);
        } catch {
          sub.lastError = 'Read unavailable; delivery paused. Check account, connectivity, and read permissions.';
        }
      }
      await this.save();
    });
  }
  private async collect(sub: Subscription): Promise<void> {
    if (sub.pending.length >= 1000) { await this.authorize(sub.space); return; }
    const found = new Map<string, Message>();
    let page: string | undefined;
    const visited = new Set<string>();
    for (let count = 0; count < 50; count++) {
      const reply = await this.read({ action: 'messages', space: sub.space, limit: 100, ...(page ? { page_token: page } : {}) });
      if (reply.account !== this.owner) throw new Error('Account mismatch');
      const messages = z.array(messageSchema).parse(reply.data.messages ?? []);
      let pastBoundary = false;
      for (const msg of messages) {
        const time = Date.parse(msg.createTime);
        if (!Number.isFinite(time) || !msg.name.startsWith(sub.space + '/messages/')) throw new Error('Invalid message');
        if (time < sub.watermark) { pastBoundary = true; continue; }
        if (time > sub.watermark || !sub.boundaryIds.includes(msg.name)) found.set(msg.name, msg);
      }
      const next = typeof reply.data.nextPageToken === 'string' ? reply.data.nextPageToken : '';
      if (pastBoundary || !next) break;
      if (count === 49 || visited.has(next)) throw new Error('Catch-up page limit; checkpoint retained');
      visited.add(next); page = next;
    }
    // Commit the oldest bounded batch. Later messages remain above the
    // checkpoint and will be picked up once queued deliveries drain.
    const messages = [...found.values()].sort((a,b) => Date.parse(a.createTime) - Date.parse(b.createTime) || a.name.localeCompare(b.name)).slice(0, 1000 - sub.pending.length);
    for (const msg of messages) {
      const time = Date.parse(msg.createTime);
      const event: Event = {
        eventId: 'evt_' + hash(JSON.stringify([this.owner, msg.name])), name: 'message.created', timestamp: msg.createTime,
        data: { space: sub.space, message: msg.name, create_time: msg.createTime, text_preview: (msg.text ?? '').slice(0, 4000), ...(msg.thread ? { thread: msg.thread.name } : {}) }, cursor: null,
      };
      if (!sub.pending.some(p => p.event.eventId === event.eventId)) sub.pending.push({ event, attempts: 0, nextAttempt: 0 });
      if (time > sub.watermark) { sub.watermark = time; sub.boundaryIds = [msg.name]; }
      else if (!sub.boundaryIds.includes(msg.name)) sub.boundaryIds.push(msg.name);
    }
    // Queue and high-water mark are committed together before any delivery.
    await this.save();
  }
  private async deliver(sub: Subscription): Promise<void> {
    for (const item of [...sub.pending]) {
      if (item.failed || item.nextAttempt > this.now()) continue;
      const body = JSON.stringify(item.event);
      const secrets = [sub.secret, ...(sub.previousSecret ? [sub.previousSecret] : [])];
      let status = 0;
      try {
        const reply = await this.post(sub.url, body, signedHeaders(item.event.eventId, sub.id, body, secrets, new Date(this.now())));
        status = reply.status;
      } catch { /* transient connection failure: persist bounded retry */ }
      if (status >= 200 && status < 300) sub.pending = sub.pending.filter(p => p !== item);
      else if (status === 410) { delete this.state.subscriptions[sub.id]; await this.save(); return; }
      else {
        item.attempts++;
        if (status === 413 || (status >= 400 && status < 500 && status !== 408 && status !== 429) || item.attempts >= 8) item.failed = true;
        item.nextAttempt = this.now() + Math.min(60 * 60_000, 1000 * 2 ** item.attempts);
        sub.lastError = item.failed ? 'Webhook delivery stopped; resubscribe after checking the destination.' : 'Webhook temporarily unavailable; retry queued.';
      }
      await this.save();
    }
  }
}
