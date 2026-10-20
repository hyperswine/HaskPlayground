import { Client } from '@modelcontextprotocol/sdk/client/index.js';
import { StdioClientTransport } from '@modelcontextprotocol/sdk/client/stdio.js';
import { fileURLToPath } from 'node:url';
import { resolve } from 'node:path';
import { writeFile } from 'node:fs/promises';
import assert from 'node:assert/strict';

const root = fileURLToPath(new URL('../', import.meta.url));
const installed = process.argv[2] ?? root;
const client = new Client({ name: 'google-chat-live-verification', version: '1.0.0' });
const transport = new StdioClientTransport({ command: '/bin/sh', args: [resolve(installed, 'launch.sh')], stderr: 'pipe' });
const results = { testedAt: new Date().toISOString(), pluginRoot: installed, checks: [] };
async function call(name, args) {
  const value = await client.callTool({ name, arguments: { ...args, response_format: 'json' } });
  if (value.isError) throw new Error(`${name}: ${value.content?.[0]?.text ?? 'failed'}`);
  assert.equal(value.structuredContent.account, 'support@vpply.com');
  return value.structuredContent;
}
try {
  await client.connect(transport);
  const { tools } = await client.listTools();
  assert.equal(tools.length, 5);
  assert.ok(tools.filter(t => t.name !== 'google_chat_send_message').every(t => t.annotations.readOnlyHint));
  results.checks.push('MCP stdio initialization and read-only discovery');
  const spaces = [];
  let cursor;
  do {
    const page = await call('google_chat_list_spaces', { limit: 2, ...(cursor ? { page_token: cursor } : {}) });
    assert.ok(page.items.length <= 2);
    spaces.push(...page.items);
    cursor = page.next_page_token;
    assert.ok(spaces.length < 100, 'unexpected space count');
  } while (cursor);
  assert.equal(new Set(spaces.map(s => s.name)).size, spaces.length);
  results.checks.push(`Spaces pagination: ${spaces.length} unique conversations`);
  const group = spaces.find(s => s.displayName === 'GregJaysonTom');
  const issues = spaces.find(s => s.displayName === 'Issues (Github)');
  assert.ok(group && issues);
  const page = await call('google_chat_read_messages', { space: group.name, limit: 2 });
  assert.ok(page.items.length > 0 && page.items.length <= 2);
  if (page.next_page_token) {
    const next = await call('google_chat_read_messages', { space: group.name, limit: 2, page_token: page.next_page_token });
    assert.ok(next.items.every(m => !page.items.some(first => first.name === m.name)));
    results.checks.push('Message pagination advances without duplicate resources');
  }
  results.checks.push('Read recent GregJaysonTom messages');
  const one = await call('google_chat_get_message', { message: page.items[0].name });
  assert.equal(one.items[0].name, page.items[0].name);
  results.checks.push('Fetched one message by resource ID');
  const thread = page.items.find(m => m.thread?.name)?.thread.name;
  assert.ok(thread);
  const replies = await call('google_chat_read_thread', { thread, limit: 5 });
  assert.ok(replies.items.length > 0);
  assert.ok(replies.items.every(m => m.thread?.name === thread));
  results.checks.push('Read thread: every returned message matches requested thread');
  const other = await call('google_chat_read_messages', { space: issues.name, limit: 3 });
  assert.ok(other.items.length > 0);
  results.checks.push(`Read Issues (Github): ${other.items.length} recent messages`);
  assert.equal(tools.find(t => t.name === 'google_chat_send_message').annotations.readOnlyHint, false);
  results.checks.push('Sender discovered; live read regression never sends messages');
  console.log(JSON.stringify(results, null, 2));
  if (process.env.GOOGLE_CHAT_TEST_REPORT) await writeFile(process.env.GOOGLE_CHAT_TEST_REPORT, JSON.stringify(results, null, 2) + '\n', { mode: 0o600 });
} finally { await client.close(); }
