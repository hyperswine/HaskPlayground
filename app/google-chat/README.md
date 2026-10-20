# Google Chat client

A small Haskell command-line client using user OAuth with a Web application
client. It lists spaces, reads messages, and sends text messages. It does not
require gcloud, and does not change gcloud's active account.

For read and send MCP tools and desktop plugin installation, see the
[local Google Chat plugin](../../plugins/google-chat-readonly/README.md).

## Setup

1. Enable Google Chat API in the OAuth client's Google Cloud project.
2. In Google Auth Platform > Clients, authorize this exact redirect URI:
   `http://localhost:8765/oauth/callback`.
   For sending, also save an app name and description in Google Chat API >
   Configuration. Leave interactive features off for this API-only client.
   This checkout uses `Haskell Chat Client`. Without this configuration,
   sending can fail with HTTP 404 even while reading works.
3. Place the downloaded Web client JSON at
   `.private/google-chat/credentials.json` (the top-level key must be `web`).
   Use `chmod 700 .private .private/google-chat` and `chmod 600` on the file.
   Google only exposes new client secrets at creation; preserve the download.
4. Build and authorize:

   ```sh
   cabal build google-chat
   cabal run google-chat -- login spaces
   cabal run google-chat -- spaces
   ```

The default account is `support@vpply.com`. Login opens Safari; complete consent
on the same Mac within five minutes. The callback binds only to 127.0.0.1:8765.
The client verifies the account's email before saving or using its session.
OAuth state rejects callbacks from other login attempts. Refresh tokens are
written atomically with owner-only permissions; access tokens stay in memory.

`GOOGLE_CHAT_CONFIG_DIR` overrides the private directory, including when running
from another working directory. `GOOGLE_CHAT_EMAIL` overrides the expected email.
The setup in this checkout imported GOOGLE_URL and GOOGLE_SECRET from the existing
`.env` directly into private JSON without printing their values. The executable
reads the JSON, not `.env`. Both `.env` and `.private/` are ignored by Git.

## Commands

```sh
cabal run google-chat -- status
cabal run google-chat -- spaces
cabal run google-chat -- login read
cabal run google-chat -- messages spaces/SPACE_ID
cabal run google-chat -- login send
cabal run google-chat -- send spaces/SPACE_ID 'Hello from Haskell'
```

`login spaces` requests OpenID/email identity and `chat.spaces.readonly` only.
`login read` also requests `chat.messages.readonly`.
`login send` also requests `chat.messages.create`. Sending posts immediately as
the authenticated user; use it only with the intended space and message.
All listing commands follow pagination and emit one JSON resource per line.
`status` shows account and scopes without displaying tokens. Each invocation
refreshes its access token. Reauthorize after revoked or expired refresh tokens.
Deleting the private session file removes the local login; revoke the app in
Google Account connections to revoke its access at Google as well.

Workspace policy may require administrator approval. An external OAuth app in
Testing can have short-lived refresh tokens; use the appropriate audience and
publishing configuration for ongoing use. This is a local client, not a hosted
multi-user gateway. No public listener is installed.

## Validation

```sh
cabal test google-chat-test --test-show-details=direct
```

The tests cover callback state, wrong paths, duplicate/missing parameters,
declined consent, resource-path validation, and scope modes. Live OAuth and API
access additionally require Google consent and the configured project.

Verified on 2026-10-19: the executable built, the validation suite passed,
Google consent completed as `support@vpply.com`, and a separate `spaces`
invocation refreshed the saved token and successfully returned five spaces.
The private response is saved in `.private/google-chat/spaces.jsonl`.
Subsequently verified message read and send scopes with the same account:
read 140 messages from `Issues (Github)` and posted one labelled test message
to `GregJaysonTom`. The initial send returned HTTP 404; saving the required
Chat app identity with interactive features off resolved it. The saved grant
now includes message reading and creation. Private API responses are kept under
`.private/google-chat/`, including `sent-test.json` and `issues-messages.jsonl`.

References: [Google OAuth web-server flow](https://developers.google.com/identity/protocols/oauth2/web-server),
[spaces.list](https://developers.google.com/workspace/chat/api/reference/rest/v1/spaces/list),
[messages.list](https://developers.google.com/workspace/chat/api/reference/rest/v1/spaces.messages/list),
[send messages](https://developers.google.com/workspace/chat/create-messages).
