import { test } from 'node:test';
import assert from 'node:assert/strict';
import { Client } from '@modelcontextprotocol/sdk/client/index.js';
import { InMemoryTransport } from '@modelcontextprotocol/sdk/inMemory.js';
import { makeServer } from '../dist/server.js';

async function connect(t, read, write = async () => { throw Error("writer must not be called"); }) {
  const server = makeServer(read, write);
  const client = new Client({ name: 'google-chat-test', version: '1.0.0' });
  const [a, b] = InMemoryTransport.createLinkedPair();
  await Promise.all([server.connect(a), client.connect(b)]);
  t.after(async () => { await client.close(); await server.close(); });
  return client;
}

test('discover four readers and one explicitly annotated sender', async t => {
  const client = await connect(t, async () => { throw Error('not called'); });
  const { tools } = await client.listTools();
  assert.deepEqual(tools.map(x => x.name).sort(), ['google_chat_get_message', 'google_chat_list_spaces', 'google_chat_read_messages', 'google_chat_read_thread', 'google_chat_send_message']);
  for (const tool of tools) {
    assert.equal(tool.annotations.readOnlyHint, tool.name !== 'google_chat_send_message');
    assert.equal(tool.annotations.destructiveHint, false);
    assert.ok(tool.inputSchema); assert.ok(tool.outputSchema);
  }
});

test('preserve page cursor and resource bounds across MCP requests', async t => {
  const queries = [];
  const client = await connect(t, async q => {
    queries.push(q);
    return { account: 'support@vpply.com', data: { spaces: [{ name: 'spaces/AAA', displayName: 'Team' }], ...(q.page_token ? {} : { nextPageToken: 'opaque+/=' }) } };
  });
  const first = await client.callTool({ name: 'google_chat_list_spaces', arguments: { limit: 1, response_format: 'json' } });
  assert.equal(first.structuredContent.next_page_token, 'opaque+/=');
  const second = await client.callTool({ name: 'google_chat_list_spaces', arguments: { limit: 1, page_token: first.structuredContent.next_page_token } });
  assert.equal(second.structuredContent.has_more, false);
  assert.deepEqual(queries, [{ action: 'spaces', limit: 1 }, { action: 'spaces', limit: 1, page_token: 'opaque+/=' }]);
});

test('read messages, thread, and one message without leaking irrelevant metadata', async t => {
  const queries = [];
  const raw = { name: 'spaces/AAA/messages/one.two', text: 'hello', createTime: '2026-10-19T00:00:00Z', thread: { name: 'spaces/AAA/threads/one' }, sender: { displayName: 'Support', avatarUrl: 'unneeded' } };
  const client = await connect(t, async q => { queries.push(q); return { account: 'support@vpply.com', data: q.action === 'message' ? raw : { messages: [raw] } }; });
  for (const [name, args] of [
    ['google_chat_read_messages', { space: 'spaces/AAA', limit: 2 }],
    ['google_chat_read_thread', { thread: 'spaces/AAA/threads/one', limit: 2 }],
    ['google_chat_get_message', { message: raw.name }],
  ]) {
    const result = await client.callTool({ name, arguments: args });
    assert.equal(result.structuredContent.items[0].text, 'hello');
    assert.equal(result.structuredContent.items[0].sender.avatarUrl, undefined);
  }
  assert.deepEqual(queries.map(q => q.action), ['messages', 'thread', 'message']);
});

test('reject path injection, oversized limits, extra arguments, and invalid sends before dispatch', async t => {
  let calls = 0;
  const client = await connect(t, async () => { calls++; throw Error('should not call'); });
  for (const [name, args] of [
    ['google_chat_read_messages', { space: 'spaces/a/../../evil' }],
    ['google_chat_read_messages', { space: 'spaces/AAA?key=bad' }],
    ['google_chat_read_messages', { space: 'spaces/AAA', limit: 101 }],
    ['google_chat_list_spaces', { action: 'send' }],
    ['google_chat_read_thread', { thread: 'spaces/AAA/threads/..' }],
    ['google_chat_send_message', { space: 'spaces/AAA', text: 'blocked' }],
  ]) {
    const result = await client.callTool({ name, arguments: args }).catch(() => ({ isError: true }));
    assert.equal(result.isError, true);
  }
  assert.equal(calls, 0);
});

test('errors never include internal exception data or credentials; session remains usable', async t => {
  const client = await connect(t, async q => {
    if (q.action === 'messages') throw Error('SECRET_TOKEN sentinel');
    return { account: 'support@vpply.com', data: { spaces: [] } };
  });
  const failed = await client.callTool({ name: 'google_chat_read_messages', arguments: { space: 'spaces/AAA' } });
  assert.equal(failed.isError, true);
  assert.ok(!JSON.stringify(failed).includes('SECRET_TOKEN'));
  const empty = await client.callTool({ name: 'google_chat_list_spaces', arguments: {} });
  assert.equal(empty.structuredContent.count, 0);
});

test('oversized pages fail explicitly, without falsely reporting complete history', async t => {
  const client = await connect(t, async () => ({ account: 'support@vpply.com', data: { messages: [{ text: 'a'.repeat(250000) }], nextPageToken: 'next' } }));
  const result = await client.callTool({ name: 'google_chat_read_messages', arguments: { space: 'spaces/AAA' } });
  assert.equal(result.isError, true);
  assert.match(result.content[0].text, /smaller limit/);
});


test('send preserves destination, exact Unicode text and retry ID; errors are sanitized', async t => {
  const queries = [];
  const args = { space: 'spaces/AAA', text: 'Morning guys, I’m testing', request_id: 'a1234567-1234-4234-8234-123456789abc' };
  const client = await connect(t, async () => { throw Error('reader must not be called'); }, async q => {
    queries.push(q);
    if (queries.length === 3) throw Error('SECRET_TOKEN');
    return { account: 'support@vpply.com', data: { name: 'spaces/AAA/messages/created', text: q.text } };
  });
  for (let n = 0; n < 2; n++) {
    const reply = await client.callTool({ name: 'google_chat_send_message', arguments: args });
    assert.equal(reply.structuredContent.message.text, args.text);
  }
  assert.deepEqual(queries, [args, args]);
  const failed = await client.callTool({ name: 'google_chat_send_message', arguments: args });
  assert.equal(failed.isError, true);
  assert.ok(!JSON.stringify(failed).includes('SECRET_TOKEN'));
  assert.match(failed.content[0].text, /same request_id/);
  for (const bad of [{ ...args, text: ' ' }, { ...args, text: '😀'.repeat(7001) }, { ...args, request_id: 'bad' }, { ...args, space: 'spaces/../evil' }, { ...args, url: 'https://evil' }]) {
    const value = await client.callTool({ name: 'google_chat_send_message', arguments: bad }).catch(() => ({ isError: true }));
    assert.equal(value.isError, true);
  }
  assert.equal(queries.length, 3);
});
