import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, readFile, stat, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { Webhook } from 'standardwebhooks';
import { EventService } from '../dist/events.js';
import { callbackUrl, publicAddress, verifyCallback } from '../dist/webhooks.js';

const owner = 'support@vpply.com';
const secret = 'whsec_' + Buffer.alloc(32, 17).toString('base64');
const input = (space = 'spaces/AAA', key = secret) => ({ name: 'message.created', arguments: { space }, delivery: { mode: 'webhook', url: 'https://receiver.example.com/events', secret: key } });
const msg = (name, time, space='spaces/AAA') => ({ name: `${space}/messages/${name}`, createTime: new Date(time).toISOString(), text: 'source content', thread: { name: `${space}/threads/one` } });
async function fixture(t) {
  const dir = await mkdtemp(join(tmpdir(), 'google-chat-events-'));
  t.after(() => rm(dir, { recursive: true, force: true }));
  let now = Date.parse('2026-10-19T00:00:00Z');
  let messages = [msg('old', now - 10000)];
  let status = 200, denied = false;
  const deliveries = [], queries = [];
  const read = async q => {
    queries.push(q);
    if (denied) throw Error('source access denied');
    const all = messages.filter(m => m.name.startsWith(q.space+'/messages/')).sort((a,b) => b.createTime.localeCompare(a.createTime));
    const start = Number(q.page_token ?? 0), limit = q.limit;
    return { account: owner, data: { messages: all.slice(start, start+limit), ...(start+limit<all.length ? {nextPageToken:String(start+limit)} : {}) } };
  };
  const post = async (url, body, headers) => {
    // Verify the production signature format, exact serialized bytes, and IDs.
    const activeKey = headers['webhook-signature'].includes(',') ? secret : secret;
    const parsed = JSON.parse(body);
    if (parsed.type === 'verification') {
      // Rotation challenges are checked in the dedicated test.
      deliveries.push({ verification: true, body: parsed, headers });
      return { status: 200, body: JSON.stringify({ challenge: parsed.challenge }) };
    }
    assert.ok(headers['webhook-signature'].split(' ').includes(new Webhook(activeKey).sign(headers['webhook-id'], new Date(Number(headers['webhook-timestamp'])*1000), body)));
    assert.equal(headers['webhook-id'], parsed.eventId);
    deliveries.push({ body: parsed, headers });
    return { status, body: '' };
  };
  const file = join(dir, 'state.json');
  const service = new EventService(file, owner, read, post, () => now);
  await service.load();
  return { service, file, read, post, deliveries, queries, clock: n=>{now+=n;}, now:()=>now,
    messages: value=>{messages=value;}, status: value=>{status=value;}, deny: value=>{denied=value;} };
}

test('reject unsafe URLs, private IPs, IPv4-mapped private IPv6, and malformed signing keys before delivery', async t => {
  for (const address of ['127.0.0.1','10.0.0.1','169.254.169.254','100.64.0.1','::1','::ffff:127.0.0.1','fc00::1','2001:db8::1']) assert.equal(publicAddress(address),false,address);
  assert.equal(publicAddress('8.8.8.8'),true);
  for (const url of ['http://example.com','https://localhost/a','https://127.0.0.1/a','https://user:pass@example.com/a','https://example.com/a#fragment']) assert.throws(()=>callbackUrl(url));
  const f = await fixture(t);
  await assert.rejects(f.service.subscribe({...input(), delivery:{...input().delivery,secret:'bad'}}));
  assert.equal(f.deliveries.length,0);
});

test('challenge mismatch fails closed and leaves no persisted subscription', async t => {
  const f=await fixture(t);
  const service = new EventService(f.file,owner,f.read,async()=>({status:200,body:'{"challenge":"wrong"}'}),f.now);
  await service.load();
  await assert.rejects(service.subscribe(input()),e=>e.code===-32015);
  assert.equal((await service.status()).subscriptions.length,0);
});

test('idempotent subscribe, private durable state, cached verification, and no historical delivery', async t => {
  const f=await fixture(t);
  const a=await f.service.subscribe(input()); const b=await f.service.subscribe(input());
  assert.equal(a.id,b.id); assert.equal(a.cursor,null);
  assert.equal(f.deliveries.filter(d=>d.verification).length,1);
  await f.service.tick();
  assert.equal(f.deliveries.filter(d=>!d.verification).length,0);
  assert.equal((await stat(f.file)).mode&0o777,0o600);
  const restart=new EventService(f.file,owner,f.read,f.post,f.now); await restart.load();
  assert.equal((await restart.status()).subscriptions[0].id,a.id);
});

test('deliver only matching new messages once; equal timestamps retain distinct IDs', async t => {
  const f=await fixture(t); await f.service.subscribe(input());
  f.clock(1000);
  f.messages([msg('new',f.now()),msg('same-time',f.now()),msg('other',f.now(),'spaces/BBB'),msg('old',f.now()-10000)]);
  await f.service.tick(); await f.service.tick();
  const events=f.deliveries.filter(d=>!d.verification);
  assert.equal(events.length,2); assert.equal(new Set(events.map(d=>d.body.eventId)).size,2);
  assert.ok(events.every(d=>d.body.data.space==='spaces/AAA'));
  assert.ok(events.every(d=>d.headers['X-MCP-Subscription-Id']));
});

test('burst pagination captures every message before advancing the checkpoint', async t => {
  const f=await fixture(t); await f.service.subscribe(input()); f.clock(1000);
  f.messages(Array.from({length:205},(_,i)=>msg('burst'+i,f.now())));
  await f.service.tick();
  assert.equal(f.deliveries.filter(d=>!d.verification).length,205);
  assert.ok(f.queries.some(q=>q.page_token==='100'));
  await f.service.tick(); assert.equal(f.deliveries.filter(d=>!d.verification).length,205);
});

test('retry persists across restart with stable event ID; revoked access pauses queued delivery', async t => {
  const f=await fixture(t); await f.service.subscribe(input()); f.clock(1000); f.messages([msg('retry',f.now())]); f.status(503);
  await f.service.tick(); const first=f.deliveries.find(d=>!d.verification);
  const restart=new EventService(f.file,owner,f.read,f.post,f.now); await restart.load();
  f.clock(5000); f.deny(true); await restart.tick();
  assert.equal(f.deliveries.filter(d=>!d.verification).length,1);
  f.deny(false); f.status(200); await restart.tick();
  const events=f.deliveries.filter(d=>!d.verification);
  assert.equal(events.length,2); assert.equal(events[1].body.eventId,first.body.eventId);
  assert.equal((await restart.status()).subscriptions[0].queued,0);
});

test('expiration, finite null TTL policy, and idempotent unsubscribe stop delivery', async t => {
  const f=await fixture(t);
  const subscribed=await f.service.subscribe({...input(),ttlMs:null}); assert.ok(subscribed.refreshBefore);
  await f.service.unsubscribe({...input(),delivery:{mode:'webhook',url:input().delivery.url}}); await f.service.unsubscribe({...input(),delivery:{mode:'webhook',url:input().delivery.url}});
  assert.equal((await f.service.status()).subscriptions.length,0);
  await f.service.subscribe({...input(),ttlMs:60000}); f.clock(61000);
  f.messages([msg('after-expiry',f.now())]); await f.service.tick();
  assert.equal(f.deliveries.filter(d=>!d.verification).length,0);
});

test('410 removes subscription; 413 stops retry; transient failures have bounded retries', async t => {
  for (const status of [410,413,503]) {
    const f=await fixture(t); await f.service.subscribe(input()); f.clock(1000); f.messages([msg('new',f.now())]); f.status(status);
    for(let i=0;i<10;i++){await f.service.tick();f.clock(3600000);}
    const count=f.deliveries.filter(d=>!d.verification).length;
    assert.equal(count,status===503?8:1);
    if(status===410) assert.equal((await f.service.status()).subscriptions.length,0);
  }
});

test('key rotation challenges the replacement secret and signs with both keys during overlap', async t => {
  const f=await fixture(t); await f.service.subscribe(input());
  const next='whsec_'+Buffer.alloc(32,23).toString('base64');
  await f.service.subscribe(input('spaces/AAA',next));
  assert.equal(f.deliveries.filter(d=>d.verification).length,2);
  f.clock(1000); f.messages([msg('rotation',f.now())]); await f.service.tick();
  const delivery=f.deliveries.find(d=>!d.verification);
  assert.equal(delivery.headers['webhook-signature'].split(' ').length,2);
  assert.ok(delivery.headers['webhook-signature'].split(' ').includes(new Webhook(next).sign(delivery.headers['webhook-id'],new Date(Number(delivery.headers['webhook-timestamp'])*1000),JSON.stringify(delivery.body))));
});

test('bounded queue catches up in batches without losing messages at the checkpoint timestamp', async t => {
  const f=await fixture(t); await f.service.subscribe(input()); f.clock(1000);
  f.messages(Array.from({length:1001},(_,i)=>msg('queued'+i,f.now())));
  await f.service.tick(); assert.equal(f.deliveries.filter(d=>!d.verification).length,1000);
  await f.service.tick(); const events=f.deliveries.filter(d=>!d.verification);
  assert.equal(events.length,1001);assert.equal(new Set(events.map(e=>e.body.eventId)).size,1001);
});

test('explicit refresh retries terminal queued deliveries with the original IDs', async t => {
  const f=await fixture(t); await f.service.subscribe(input()); f.clock(1000);
  f.messages([msg('failed',f.now())]); f.status(413);await f.service.tick();
  assert.equal((await f.service.status()).subscriptions[0].failed,1);
  const first=f.deliveries.find(d=>!d.verification).body.eventId;
  f.status(200);await f.service.subscribe(input());await f.service.tick();
  assert.equal(f.deliveries.filter(d=>!d.verification)[1].body.eventId,first);
  assert.equal((await f.service.status()).subscriptions[0].failed,0);
});
