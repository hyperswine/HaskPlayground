import { execFileSync } from 'node:child_process';
import { readFileSync, mkdirSync, writeFileSync, chmodSync } from 'node:fs';
import { dirname, resolve, join } from 'node:path';
import { homedir } from 'node:os';
import { fileURLToPath } from 'node:url';

if(process.platform!=='darwin') throw new Error('This installer targets macOS launchd. Run dist/worker.js under your service manager elsewhere.');
const root=process.argv[2] ? resolve(process.argv[2]) : dirname(fileURLToPath(import.meta.url));
const config=JSON.parse(readFileSync(resolve(root,'runtime.json'),'utf8'));
const dir=resolve(config.configDir,'events');
mkdirSync(dir,{recursive:true,mode:0o700});chmodSync(dir,0o700);
for(const file of ['stdout.log','stderr.log']) writeFileSync(join(dir,file),'',{flag:'a',mode:0o600});
const label='com.haskplayground.google-chat-events';
const domain=`gui/${process.getuid()}`;
const target=`${domain}/${label}`;
const agents=join(homedir(),'Library/LaunchAgents');mkdirSync(agents,{recursive:true});
const plist=join(agents,label+'.plist');
const xml=value=>String(value).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;').replace(/'/g,'&apos;');
const values=[process.execPath,resolve(root,'dist/worker.js')];
writeFileSync(plist,`<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>Label</key><string>${label}</string>
<key>ProgramArguments</key><array>${values.map(v=>`<string>${xml(v)}</string>`).join('')}</array>
<key>WorkingDirectory</key><string>${xml(root)}</string>
<key>RunAtLoad</key><true/><key>KeepAlive</key><true/>
<key>ThrottleInterval</key><integer>15</integer>
<key>Umask</key><integer>63</integer>
<key>StandardOutPath</key><string>${xml(join(dir,'stdout.log'))}</string>
<key>StandardErrorPath</key><string>${xml(join(dir,'stderr.log'))}</string>
</dict></plist>
`,{mode:0o600});
execFileSync('/usr/bin/plutil',['-lint',plist],{stdio:'inherit'});
try{execFileSync('/bin/launchctl',['print',target],{stdio:'ignore'});execFileSync('/bin/launchctl',['bootout',target],{stdio:'ignore'});}catch{/* no previous instance */}
execFileSync('/bin/launchctl',['bootstrap',domain,plist],{stdio:'inherit'});
console.log(`Started ${label}. It stays idle until Dot subscribes; private status is ${join(dir,'status.json')}.`);
