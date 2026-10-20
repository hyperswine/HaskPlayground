import { createConnection } from 'node:net';
import { join } from 'node:path';
import { z } from 'zod';
import { McpServer, ProtocolError, type ServerCapabilities } from '@modelcontextprotocol/server';
import { eventDefinition, subscribeSchema, unsubscribeSchema } from './event-schema.js';

export type EventMethod = 'subscribe' | 'unsubscribe' | 'status';
export function eventSocket(): string {
  const dir = process.env.GOOGLE_CHAT_CONFIG_DIR;
  if (!dir?.startsWith('/')) throw new Error('Absolute Google Chat configuration directory required');
  return join(dir, 'events', 'worker.sock');
}
export async function eventRpc(method: EventMethod, params: unknown): Promise<unknown> {
  return new Promise((resolve, reject) => {
    const socket = createConnection(eventSocket());
    let data = '';
    const timer = setTimeout(() => socket.destroy(new Error('Event worker timed out')), 90_000);
    socket.setEncoding('utf8');
    socket.on('connect', () => socket.write(JSON.stringify({ method, params }) + '\n'));
    socket.on('data', chunk => {
      data += chunk;
      if (Buffer.byteLength(data) > 128 * 1024) { socket.destroy(new Error('Invalid worker response')); return; }
      if (!data.includes('\n')) return;
      try {
        const reply = JSON.parse(data.slice(0, data.indexOf('\n')));
        if (reply.error) reject(new ProtocolError(reply.error.code, reply.error.message, { reason: reply.error.reason }));
        else resolve(reply.result);
      } catch { reject(new Error('Event worker response failed')); }
      socket.end();
    });
    socket.on('close', () => clearTimeout(timer));
    socket.on('error', () => reject(new ProtocolError(-32603, 'Local event worker unavailable. Start the Google Chat events service using the plugin setup guide.')));
  });
}
export function registerEvents(server: McpServer, rpc = eventRpc): void {
  const capabilities: ServerCapabilities & { events: Record<string, never> } = { events: {} };
  server.server.registerCapabilities(capabilities);
  server.server.setRequestHandler('events/list', {
    params: z.object({ cursor: z.string().optional() }).passthrough().optional(),
    result: z.object({ events: z.array(z.unknown()) }),
  }, async () => ({ events: [eventDefinition] }));
  server.server.setRequestHandler('events/subscribe', {
    params: subscribeSchema,
    result: z.object({ id: z.string(), refreshBefore: z.string().nullable(), cursor: z.null(), truncated: z.boolean() }),
  }, async params => z.object({ id: z.string(), refreshBefore: z.string().nullable(), cursor: z.null(), truncated: z.boolean() }).parse(await rpc('subscribe', params)));
  server.server.setRequestHandler('events/unsubscribe', {
    params: unsubscribeSchema, result: z.object({}).strict(),
  }, async params => { await rpc('unsubscribe', params); return {}; });
}
