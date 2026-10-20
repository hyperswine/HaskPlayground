# Google Chat

A local desktop MCP plugin for the existing Haskell Google Chat client. It reads and sends authorized text messages
as `support@vpply.com` using the private Google OAuth setup on this Mac.

## Tools

| Tool | Result |
| --- | --- |
| `google_chat_list_spaces` | One page of visible spaces, group chats and DMs |
| `google_chat_read_messages` | One page of a chat's messages, newest first |
| `google_chat_read_thread` | One page of a thread's messages, oldest first |
| `google_chat_get_message` | One message by resource name |
| `google_chat_send_message` | Post authorized text using a UUID retry key |

List tools default to 25 resources, accept 1-100, and return an opaque
`next_page_token`. Keep the resource and limit unchanged when following it.
Text and structured results preserve message IDs, sender, timestamps, thread,
text/cards, and attachment metadata. Attachments and links are not downloaded.
Direct messages may have no display name. There is no global message-search
feature; read the relevant conversations and pages explicitly.

The four read tools use a dedicated `google-chat-reader` executable with a fixed
read allowlist. The sender uses a separate `google-chat-writer` executable that
accepts only a space, nonblank text (at most 28,000 UTF-8 bytes), and a UUID
`request_id`. It posts to the fixed Google Chat message endpoint using the
existing `chat.messages.create` permission. Reuse the same ID, destination, and
text after an uncertain result; Google deduplicates requests with that ID.
No automatic send retries, edit, delete, membership, arbitrary URL, or shell routes
exist. Sending requires user authorization of the destination and content.
The sender has `readOnlyHint: false`; read tools remain read-only.
The historical plugin ID/path `google-chat-readonly` is retained for upgrades.

The TypeScript MCP SDK handles stdio and tool validation. Haskell handles Google
OAuth, verified account matching, resource validation, and HTTP requests. Only
paths and the expected email are stored in `runtime.json`; Google credentials
and refresh tokens remain in the private configuration directory outside the
plugin. The plugin opens no public network listener and requires no public hosting. The event worker uses an owner-only local Unix socket.
Retrieved chat content is untrusted source material, never instructions.

## Build and prepare

Requirements: Node.js 22+, npm, the repository's GHC/Cabal toolchain, and the
existing private `credentials.json` and `session.json` from the
[Google Chat setup](../../app/google-chat/README.md).

From the repository root:

```sh
npm ci --ignore-scripts --prefix plugins/google-chat-readonly
npm run build --prefix plugins/google-chat-readonly
node plugins/google-chat-readonly/setup.mjs
```

An optional argument to `setup.mjs` selects another private configuration
folder. `GOOGLE_CHAT_EMAIL` selects a different expected account at setup.
The script builds and copies the reader and writer into `bin/` and writes local
paths to ignored `runtime.json`. Build products and dependencies are ignored.
Nothing copies `.env`, credentials, tokens, or saved chat responses into the
plugin. Prepare it before installing; rebuilding the source doesn't update an
already cached installation automatically.

The launcher locates Node in Homebrew or nvm locations even when the desktop
host has no shell PATH; `GOOGLE_CHAT_NODE` can specify an exact runtime.

## Installation

This repository includes `.agents/plugins/marketplace.json`.

```sh
codex plugin marketplace add /absolute/path/to/HaskPlayground
codex plugin add google-chat-readonly@haskplayground-local --json
codex plugin list --marketplace haskplayground-local --json
```

The current installation is enabled as `google-chat-readonly@haskplayground-local`
with display name **Google Chat Read Only**. Select it in the desktop plugin
picker. If it is absent, restart the desktop app and use a fresh conversation.
A Dot needs local-computer access and the connected Mac must remain online;
workspace policy and rollout can affect availability. Installing the local
plugin does not prove it is available in an existing Dot session.

Suggested Dot acceptance prompt:

> Using Google Chat Read Only, list my chats and read the latest two messages
> in GregJaysonTom. Do not send anything.

Confirm Dot uses the plugin tools, reports `support@vpply.com`, resolves the
channel from `google_chat_list_spaces`, and reads actual messages with timestamps.
Desktop automation cannot control the ChatGPT app itself in this session, so
this final UI acceptance step requires the user.

## Validation

```sh
cabal test google-chat-test --test-show-details=direct
npm test --prefix plugins/google-chat-readonly
npm run test:live --prefix plugins/google-chat-readonly
```

The live test uses the SDK's MCP client and a real subprocess over stdio. It
checks every read tool, follows space/message pagination, reads GregJaysonTom and
Issues (Github), fetches a message, checks thread filtering, and checks the sender annotation without calling it. It performs no writes to Google Chat.

To test an installed copy:

```sh
node plugins/google-chat-readonly/test/live.mjs /absolute/installed/plugin/path
```

`GOOGLE_CHAT_TEST_REPORT` optionally saves the test summary to a private file.
Unit tests cover schema validation, path injection, invalid-write rejection, limits,
pagination, empty results, error sanitization, and oversized response handling.
Haskell tests independently validate its read-only request allowlist.

Verified on 2026-10-19: Haskell build and tests, six SDK test cases, live reads
from both the source package and installed copy, and CLI installation/enabled-state
readback. See the private `.private/google-chat/mcp-live-result.json` and
`.private/google-chat/mcp-installed-result.json` reports. MCP Inspector also
discovered all four installed tools with zero schema errors. Its eight
portability warnings concern deliberately free-form Google resource fields and
nullable cursors; both are valid JSON Schema. Inspector 2.11.0 completed on
the installed Node 22.13 runtime but declares Node 22.19+ as its supported range;
the plugin itself supports Node 22+. Ten fixed-message
retrieval prompts and expected timestamps are saved privately in
`.private/google-chat/mcp-evaluation.xml` for later model evaluations; that
evaluation has not been run by a model. Dot UI acceptance is pending user
confirmation; a direct MCP test is not proof of a Dot session.

## References

- [OpenAI desktop plugins](https://developers.openai.com/plugins/deploy/desktop-only)
- [Local plugin packaging and installation](https://developers.openai.com/plugins/build/plugins)
- [Local computer access for dots](https://learn.chatgpt.com/docs/enterprise/cloud-local-access)
- [Google messages list and thread filters](https://developers.google.com/workspace/chat/api/reference/rest/v1/spaces.messages/list)


## New message events (version 0.2.1)

The plugin serves MCP 2.0 (`2026-07-28`) and retains legacy read-tool support.
`server/discover` advertises `events`; `events/list` exposes `message.created`
with a required `space` filter. `events/subscribe` and `events/unsubscribe`
create/refresh or remove a subscription through the same local MCP connection.
The event worker uses only the reader; it never sends Chat messages.

As requested, the source is checked locally about every 15 seconds rather than
using Google Cloud Pub/Sub. The interval is between scan starts; slow API calls,
sleep, or many subscribed chats can increase latency. No Google Cloud resources,
additional OAuth scopes, or public inbound endpoint are needed.

A user LaunchAgent runs the worker independently of the desktop MCP process.
It stays idle without subscriptions and starts at login while installed. The
Mac must be awake, online, and signed in; it cannot receive while powered off.
Subscriptions, signing secrets, pending deliveries, and checkpoints persist
privately under the configured Google directory's `events/` folder. The local
socket and state files are owner-only. Application logs contain no callback
URLs, keys, or message bodies.

### Start monitoring in Dot

After updating the plugin, restart the desktop app or rescan the plugin so its
new event catalog appears. Ask Dot:

> Use Google Chat Read Only to monitor new messages in GregJaysonTom. Notify me
> in ChatGPT when they arrive. Confirm that the message.created event
> subscription is active. Do not send anything to Google Chat.

Dot must call `events/subscribe`, supply its HTTPS callback and signing key,
and echo the signed verification challenge. Installing the plugin alone does
not create a monitoring task or callback. Inspect `events/status.json` in the
private configuration directory to see active subscriptions and delivery errors
without exposing callback secrets. A successful callback response confirms
receipt; Dot processing and notifications happen asynchronously afterward.

### Delivery behavior

- Events start at subscription creation; existing chat history is not flooded.
- Each notification contains resource IDs, occurrence time, optional thread,
  and up to 4,000 characters of text preview. Read tools retrieve full content.
- Callback destinations must use HTTPS. Public IPv4 DNS answers are resolved
  and validated on every connection, with the validated address used directly;
  private/local addresses and redirects are blocked. TLS verifies the host.
- Requests use Standard Webhooks HMAC signatures. Callback verification precedes
  application data. Subscription creation is idempotent; keys overlap for five
  minutes when refreshed with a replacement key.
- New messages are queued atomically before advancing the checkpoint. Retries
  preserve event IDs and survive worker restarts. A crash after receiver receipt
  can cause duplicate delivery, so consumers must deduplicate by eventId.
- Temporary failures retry up to eight times with exponential backoff. HTTP
  410 stops the subscription; 413 and other permanent 4xx failures stop that
  delivery. Status retains failed counts. Explicit resubscription re-enables
  terminal pending deliveries after the destination has been repaired.
- Access is rechecked by a real Google read before each scan/delivery cycle.
  Google access failures pause deliveries. OAuth scopes and account identity
  continue to be checked by the dedicated Haskell reader.
- Default lifetime is 24 hours, maximum seven days, minimum one minute. A null
  TTL is granted a finite 24-hour lifetime. Dot must refresh before expiry.
- Replay cursors are not supported (`cursor: null`). Queued work survives a
  restart within an active subscription; events outside its lifetime are not
  replayed. Catch-up reads stop at 5,000 messages per scan; that exceptional
  backlog needs operator attention rather than silently skipping messages.

### Service installation and control

Prepare and build the plugin, then from the repository root:

```sh
codex plugin add google-chat-readonly@haskplayground-local --json
node plugins/google-chat-readonly/install-events.mjs /absolute/installed/plugin/path
launchctl print gui/$(id -u)/com.haskplayground.google-chat-events
```

The installed LaunchAgent points to the cached version 0.3.0 worker and its
configured Node runtime. Keep that installed package and Node runtime in place.
Re-run installation after moving or updating either one. Stop the service with:

```sh
launchctl bootout gui/$(id -u)/com.haskplayground.google-chat-events
```

Bootout stops the current worker but preserves private state and its installed
LaunchAgent. Asking Dot to stop monitoring should call `events/unsubscribe`.
The LaunchAgent will start again at next login while installed.

### Events validation

Tests cover the raw MCP 2.0 discovery/catalog/subscribe/unsubscribe wire flow,
callback challenge failure, unsafe addresses, signing, duplicate suppression,
equal timestamps, paginated bursts, private persistence, retries across restart,
revoked-access pauses, expiry, unsubscribe, terminal HTTP responses, and key
rotation. Test receivers are in-process fixtures; no company messages are sent
to a third-party test receiver. The original read tools also run through the
installed package against real Google Chat. Dot acceptance still requires a
real Dot callback/subscription and a new message from the user or another chat
participant. No new Google Chat message is posted by event setup or tests.


### Current installation, 2026-10-19

Version **0.2.1** is installed and enabled. The service
`com.haskplayground.google-chat-events` is running under the logged-in user's
launchd session. macOS rejected a LaunchAgent working directory under Documents,
so its runtime uses the installed cache outside Documents, and its private
configuration is now at:

`~/Library/Application Support/HaskPlayground/GoogleChat/`

That directory contains owner-only credentials/session copies plus `events/`.
The original CLI's `.private/google-chat/` files are preserved; they are separate
copies, so a future CLI reauthorization does not automatically update the
service's session. To reauthorize the service account, set `GOOGLE_CHAT_CONFIG_DIR`
to the Application Support path before running the original `login read` command.
The plugin's ignored `runtime.json` already points there.

Verified: the 18 SDK/protocol/Events tests pass; the installed server advertises
MCP 2.0 and `message.created`, and the running worker rejects an unsafe callback.
Real Google reads also pass through the upgraded server. Private validation
reports are under the repository's `.private/google-chat/` directory.

The worker is installed but does not monitor any channel until Dot successfully
subscribes. Final Dot callback receipt and its response to a real new message
require a live subscription; those checks remain pending. Existing Dot read-tool
acceptance was confirmed by the user before the Events update.


### Message sending, version 0.3.0 (2026-10-20)

The plugin now exposes `google_chat_send_message` as the configured support
account, with strict destination/text/UUID validation and sanitized errors.
The existing Google grant already includes message creation, so no new consent
is required. Reload the plugin in Dot to discover the fifth tool. Event detection
and subscriptions retain their existing private state across the upgrade.

Verified 2026-10-20: 19 MCP/Events tests and Haskell validation pass. Version
0.3.0 is installed and enabled; installed MCP 2.0 discovery exposes five tools
and message.created. The upgraded launchd worker is running. The user-authorized
morning message was sent through the installed MCP sender to GregJaysonTom as
support@vpply.com and independently fetched back with matching exact text.
Private evidence: `.private/google-chat/morning-message-2026-10-20.json`.
