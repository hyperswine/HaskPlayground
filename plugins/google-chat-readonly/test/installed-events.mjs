import { spawn } from 'node:child_process';
import assert from 'node:assert/strict';
import { resolve } from 'node:path';
import { readFile, writeFile } from 'node:fs/promises';
import { randomBytes } from 'node:crypto';

const root=resolve(process.argv[2]);
const child=spawn('/bin/sh',[resolve(root,'launch.sh')],{stdio:['pipe','pipe','pipe']});
child.stderr.resume();
let buffer='';const pending=new Map();
child.stdout.on('data',chunk=>{
  buffer+=chunk;
  while(buffer.includes('\n')){
    const at=buffer.indexOf('\n');const reply=JSON.parse(buffer.slice(0,at));buffer=buffer.slice(at+1);
    if(pending.has(reply.id)){pending.get(reply.id)(reply);pending.delete(reply.id);}
  }
});
const meta={'io.modelcontextprotocol/protocolVersion':'2026-07-28','io.modelcontextprotocol/clientCapabilities':{}};
let id=0;
async function call(method,params={}){
  const next=++id;
  return new Promise((accept,reject)=>{
    const timer=setTimeout(()=>reject(Error('Installed event endpoint timed out')),15000);
    pending.set(next,reply=>{clearTimeout(timer);accept(reply);});
    child.stdin.write(JSON.stringify({jsonrpc:'2.0',id:next,method,params:{...params,_meta:meta}})+'\n');
  });
}
try {
  const discover=await call('server/discover');
  assert.ok(discover.result.supportedVersions.includes('2026-07-28'));
  assert.deepEqual(discover.result.capabilities.events,{});
  const listed=await call('events/list');assert.equal(listed.result.events[0].name,'message.created');
  const tools=await call('tools/list');assert.equal(tools.result.tools.length,5);
  // Reject unsafe delivery destinations before Google reads, state changes,
  // or outbound application data. No actual Dot signing secret is involved.
  const invalid=await call('events/subscribe',{name:'message.created',arguments:{space:'spaces/AAQAGgA4Uc4'},delivery:{mode:'webhook',url:'https://127.0.0.1/events',secret:'whsec_'+randomBytes(32).toString('base64')}});
  assert.equal(invalid.error.code,-32602);
  const runtime=JSON.parse(await readFile(resolve(root,'runtime.json'),'utf8'));
  const status=JSON.parse(await readFile(resolve(runtime.configDir,'events/status.json'),'utf8'));
  assert.ok(Date.now()-Date.parse(status.checkedAt)<60000);
  const report={testedAt:new Date().toISOString(),pluginRoot:root,protocol:'2026-07-28',events:['message.created'],tools:5,unsafeCallbackRejected:true,workerLastChecked:status.checkedAt,activeSubscriptions:status.subscriptions.length};
  console.log(JSON.stringify(report,null,2));
  if(process.env.GOOGLE_CHAT_TEST_REPORT) await writeFile(process.env.GOOGLE_CHAT_TEST_REPORT,JSON.stringify(report,null,2)+'\n',{mode:0o600});
} finally { child.stdin.end(); child.kill('SIGTERM'); }
