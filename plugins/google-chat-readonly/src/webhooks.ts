import { request } from 'node:https';
import { lookup as dnsLookup } from 'node:dns';
import { isIP } from 'node:net';
import ipaddr from 'ipaddr.js';
import { randomBytes, timingSafeEqual } from 'node:crypto';
import { Webhook } from 'standardwebhooks';

export type WebhookReply = { status: number; body: string };
export type WebhookPost = (url: string, body: string, headers: Record<string, string>) => Promise<WebhookReply>;
export function publicAddress(address: string): boolean {
  try { return ipaddr.process(address).range() === 'unicast'; } catch { return false; }
}
export function callbackUrl(value: string): URL {
  const url = new URL(value);
  const host = url.hostname.replace(/^\[|\]$/g, '');
  if (url.protocol !== 'https:' || url.username || url.password || url.hash ||
      host === 'localhost' || host.endsWith('.localhost') ||
      (isIP(host) && !publicAddress(host))) throw new Error('Unsafe callback');
  return url;
}

// Re-resolve on EVERY connection, validate ALL answers, and connect directly to
// the validated address through lookup. TLS still verifies the original host.
export const securePost: WebhookPost = async (value, body, headers) => {
  const url = callbackUrl(value);
  return new Promise((resolve, reject) => {
    const req = request(url, {
      method: 'POST', headers: { ...headers, 'Content-Length': Buffer.byteLength(body).toString() },
      agent: false, family: 4,
      lookup(host, _options, callback) {
        dnsLookup(host, { all: true, family: 4, verbatim: true }, (error, addresses) => {
          if (error || !addresses.length || addresses.some(a => !publicAddress(a.address))) {
            callback(new Error('Callback destination blocked'), '', 4); return;
          }
          callback(null, addresses[0].address, addresses[0].family);
        });
      },
    }, response => {
      let data = '';
      response.setEncoding('utf8');
      response.on('data', (chunk: string) => {
        data += chunk;
        if (Buffer.byteLength(data) > 8192) { response.destroy(); req.destroy(new Error('Response too large')); }
      });
      response.on('error', reject);
      response.on('end', () => resolve({ status: response.statusCode ?? 0, body: data }));
    });
    // Absolute deadline includes DNS, connection, TLS, and response time.
    const timer = setTimeout(() => req.destroy(new Error('Callback timeout')), 10000);
    req.on('close', () => clearTimeout(timer));
    req.on('error', () => reject(new Error('Callback connection failed')));
    req.end(body);
  });
};

export function signingSecret(secret: string): string {
  if (!/^whsec_[A-Za-z0-9+/]+={0,2}$/.test(secret)) throw new Error('Invalid signing secret');
  const bytes = Buffer.from(secret.slice(6), 'base64');
  if (bytes.length < 24 || bytes.length > 64 || bytes.toString('base64').replace(/=+$/, '') !== secret.slice(6).replace(/=+$/, ''))
    throw new Error('Invalid signing secret');
  return secret;
}
export function signedHeaders(id: string, subscriptionId: string, body: string, secrets: string[], now = new Date()): Record<string, string> {
  return {
    'Content-Type': 'application/json', 'webhook-id': id,
    'webhook-timestamp': String(Math.floor(now.getTime() / 1000)),
    'webhook-signature': secrets.map(secret => new Webhook(signingSecret(secret)).sign(id, now, body)).join(' '),
    'X-MCP-Subscription-Id': subscriptionId,
  };
}
export async function verifyCallback(url: string, secret: string, subscriptionId: string, post: WebhookPost = securePost): Promise<void> {
  callbackUrl(url); signingSecret(secret);
  const challenge = randomBytes(32).toString('hex');
  const body = JSON.stringify({ type: 'verification', challenge });
  const reply = await post(url, body, signedHeaders(`verify_${randomBytes(16).toString('hex')}`, subscriptionId, body, [secret]));
  if (reply.status < 200 || reply.status >= 300) throw new Error('Callback verification failed');
  let echo: unknown;
  try { echo = JSON.parse(reply.body).challenge; } catch { throw new Error('Callback challenge failed'); }
  if (typeof echo !== 'string' || Buffer.byteLength(echo) !== Buffer.byteLength(challenge) ||
      !timingSafeEqual(Buffer.from(echo), Buffer.from(challenge))) throw new Error('Callback challenge failed');
}
