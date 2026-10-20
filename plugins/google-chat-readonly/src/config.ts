import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { z } from 'zod';
export async function configure(): Promise<{ configDir: string; email: string }> {
  const file = fileURLToPath(new URL('../runtime.json', import.meta.url));
  const config = z.object({ configDir: z.string().startsWith('/'), email: z.string().email() }).strict()
    .parse(JSON.parse(await readFile(file, 'utf8')));
  process.env.GOOGLE_CHAT_CONFIG_DIR = config.configDir;
  process.env.GOOGLE_CHAT_EMAIL = config.email;
  return config;
}
