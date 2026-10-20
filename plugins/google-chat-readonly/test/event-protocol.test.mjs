import { test } from 'node:test';
import assert from 'node:assert/strict';
import { PassThrough } from 'node:stream';
import { serveStdio, StdioServerTransport } from '@modelcontextprotocol/server/stdio';
import { registerEvents } from '../dist/event-ipc.js';
import { makeServer } from '../dist/server.js';

// Inspect raw wire bytes so a client SDK cannot strip an extension capability.
test('MCP 2.0 discover advertises events, with event catalog on the same stdio endpoint', async t => {
  const input = new PassThrough(), output = new PassThrough();
  const handle = serveStdio(() => { const server=makeServer(async()=>{throw Error('no Google read needed');}); registerEvents(server, async (method) => method==='subscribe' ? {id:'sub_test',refreshBefore:new Date(Date.now()+60000).toISOString(),cursor:null,truncated:false} : {}); return server; }, {transport:new StdioServerTransport(input,output)});
  t.after(async()=>{await handle.close();input.destroy();output.destroy();});
  const lines=[]; let buffered='';
  output.on('data',chunk=>{buffered+=chunk.toString();while(buffered.includes('\n')){const at=buffered.indexOf('\n');lines.push(JSON.parse(buffered.slice(0,at)));buffered=buffered.slice(at+1);}});
  const meta = { 'io.modelcontextprotocol/protocolVersion': '2026-07-28', 'io.modelcontextprotocol/clientCapabilities': {} };
  async function send(id,method,params={}) {
    input.write(JSON.stringify({jsonrpc:'2.0',id,method,params:{...params,_meta:meta}})+'\n');
    for(let i=0;i<100;i++){const found=lines.find(x=>x.id===id);if(found)return found;await new Promise(r=>setTimeout(r,10));}
    throw Error('No response');
  }
  const discovery=await send(1,'server/discover');
  assert.ok(discovery.result,JSON.stringify(discovery));
  assert.ok(discovery.result.supportedVersions.includes('2026-07-28'));
  assert.deepEqual(discovery.result.capabilities.events,{});
  const events=await send(2,'events/list');
  assert.equal(events.result.events[0].name,'message.created');
  assert.deepEqual(events.result.events[0].delivery,['webhook']);
  const subscribed=await send(3,'events/subscribe',{name:'message.created',arguments:{space:'spaces/AAA'},delivery:{mode:'webhook',url:'https://receiver.example.com/events',secret:'whsec_'+Buffer.alloc(32,5).toString('base64')}});
  assert.equal(subscribed.result?.id,'sub_test',JSON.stringify(subscribed));
  const stopped=await send(4,'events/unsubscribe',{name:'message.created',arguments:{space:'spaces/AAA'},delivery:{mode:'webhook',url:'https://receiver.example.com/events'}});
  assert.ok(stopped.result,JSON.stringify(stopped));
});
