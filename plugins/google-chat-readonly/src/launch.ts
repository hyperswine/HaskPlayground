import { makeServer } from './server.js';
import { serveStdio } from '@modelcontextprotocol/server/stdio';
import { configure } from './config.js';
try {
  await configure();
  serveStdio(() => makeServer());
} catch {
  console.error('Google Chat plugin setup is missing or invalid; follow the local README and rerun setup.');
  process.exitCode = 1;
}
