import { execFileSync } from 'node:child_process';
import { copyFileSync, mkdirSync, writeFileSync, chmodSync, existsSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = dirname(fileURLToPath(import.meta.url));
const repo = resolve(root, '../..');
const configDir = resolve(process.argv[2] ?? resolve(repo, '.private/google-chat'));
for (const file of ['credentials.json', 'session.json']) {
  if (!existsSync(resolve(configDir, file))) throw new Error(`Missing private ${file}; finish Google OAuth setup first.`);
}
for (const name of ['google-chat-reader', 'google-chat-writer']) {
  execFileSync('cabal', ['build', name], { cwd: repo, stdio: 'inherit' });
  const binary = execFileSync('cabal', ['list-bin', name], { cwd: repo, encoding: 'utf8' }).trim();
  mkdirSync(resolve(root, 'bin'), { recursive: true });
  copyFileSync(binary, resolve(root, 'bin', name));
  chmodSync(resolve(root, 'bin', name), 0o755);
}
writeFileSync(resolve(root, 'runtime.json'), JSON.stringify({ configDir, email: process.env.GOOGLE_CHAT_EMAIL ?? 'support@vpply.com' }, null, 2) + '\n', { mode: 0o600 });
console.log('Local reader and sender prepared. Credentials remain outside the plugin package.');
