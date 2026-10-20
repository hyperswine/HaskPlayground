import { createServer, createConnection } from 'node:net';
import { mkdir, chmod, unlink, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { z } from 'zod';
import { configure } from './config.js';
import { eventSocket } from './event-ipc.js';
import { EventService, EventError } from './events.js';
import { runReader } from './server.js';

const config = await configure();
const dir = join(config.configDir, 'events');
await mkdir(dir, { recursive: true, mode: 0o700 });
await chmod(dir, 0o700);
const socketPath = eventSocket();
// Never remove another live worker's socket. A stale socket is removed only
// after a connection proves there is no listener.
const active = await new Promise<boolean>((resolve, reject) => {
  const probe = createConnection(socketPath);
  probe.on('connect', () => { probe.destroy(); resolve(true); });
  probe.on('error', (error: NodeJS.ErrnoException) => {
    if (error.code === 'ENOENT' || error.code === 'ECONNREFUSED') resolve(false); else reject(error);
  });
});
if (active) throw new Error('An event worker is already running');
await unlink(socketPath).catch((error: NodeJS.ErrnoException) => { if (error.code !== 'ENOENT') throw error; });
const service = new EventService(join(dir, 'state.json'), config.email, runReader);
await service.load();
const requestSchema = z.object({ method: z.enum(['subscribe', 'unsubscribe', 'status']), params: z.unknown() }).strict();
const server = createServer(socket => {
  let input = ''; let handled = false;
  socket.setTimeout(95_000, () => socket.destroy());
  socket.setEncoding('utf8'); socket.on('error', () => {});
  socket.on('data', chunk => {
    if (handled) return;
    input += chunk;
    if (Buffer.byteLength(input) > 16384) { socket.destroy(); return; }
    if (!input.includes('\n')) return;
    handled = true;
    void (async () => {
      try {
        const req = requestSchema.parse(JSON.parse(input.slice(0, input.indexOf('\n'))));
        const result = req.method === 'subscribe' ? await service.subscribe(req.params)
          : req.method === 'unsubscribe' ? await service.unsubscribe(req.params) : await service.status();
        socket.end(JSON.stringify({ result }) + '\n');
      } catch (error) {
        const safe = error instanceof EventError ? { code: error.code, message: error.message, reason: error.reason }
          : { code: -32602, message: 'Event request failed; check arguments, Google access, and callback verification.', reason: 'request_failed' };
        socket.end(JSON.stringify({ error: safe }) + '\n');
      }
    })();
  });
});
await new Promise<void>((resolve, reject) => { server.once('error', reject); server.listen(socketPath, resolve); });
await chmod(socketPath, 0o600);
let stopping = false;
let wake: (() => void) | undefined;
async function tick(): Promise<void> {
  try {
    await service.tick();
    await writeFile(join(dir, 'status.json'), JSON.stringify({ checkedAt: new Date().toISOString(), ...await service.status() }, null, 2) + '\n', { mode: 0o600 });
  } catch { console.error('Google Chat event cycle failed; private state retained.'); }
}
void (async () => {
  while (!stopping) {
    const started = Date.now();
    await tick();
    if (!stopping) await new Promise<void>(resolve => { const timer = setTimeout(resolve, Math.max(0, 15000 - (Date.now() - started))); wake = () => { clearTimeout(timer); resolve(); }; });
  }
})();
for (const signal of ['SIGTERM', 'SIGINT'] as const) process.once(signal, () => {
  stopping = true; wake?.(); server.close();
  // In-flight state commits finish naturally; launchd provides a bounded stop.
});
console.error('Google Chat event worker ready; subscriptions are private and durable.');
