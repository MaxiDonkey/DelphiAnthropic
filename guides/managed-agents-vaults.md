# Vaults & Credentials [beta]

**Vaults** are workspace-scoped containers that store secrets reused by managed sessions: MCP-server access tokens, OAuth refresh tokens, static bearer tokens. Each Vault owns a nested set of **Credentials**, each carrying a typed authentication block (`mcp_oauth` or `static_bearer`).

- [Overview](#overview)
- [Create a vault](#create-a-vault)
- [Retrieve / list vaults](#retrieve--list-vaults)
- [Update a vault](#update-a-vault)
- [Archive / delete a vault](#archive--delete-a-vault)
- [Create a credential — static bearer](#create-a-credential--static-bearer)
- [Create a credential — MCP OAuth (access token only)](#create-a-credential--mcp-oauth-access-token-only)
- [Create a credential — MCP OAuth with refresh](#create-a-credential--mcp-oauth-with-refresh)
- [Retrieve / list credentials](#retrieve--list-credentials)
- [Update a credential (rotate secret, edit metadata)](#update-a-credential-rotate-secret-edit-metadata)
- [Archive / delete a credential](#archive--delete-a-credential)
- [Validate a credential](#validate-a-credential)
- [Attaching vaults to a session](#attaching-vaults-to-a-session)
- [Webhook events](#webhook-events)
- [Constraints & notes](#constraints--notes)
- [References](#references)

___

<br>

## Overview

The wrapper exposes two routes hanging off `Client.Vaults`:

| Sub-route | Wrapper accessor | Purpose |
|---|---|---|
| Vaults | `Client.Vaults` | CRUD + archive on vaults |
| Credentials | `Client.Vaults.Credentials` | CRUD + archive + validate on credentials within a vault |

All operations have both a blocking variant and a promise-based variant (`AsyncAwait*`). The `Generation.Vault` helper (alias `Vault`) and its nested `Vault.Credential` helper expose factory methods for every params class.

Vaults are typically referenced indirectly: a session declares the vault ids it needs through `VaultIds([...])`, and the server resolves the actual secrets from those vaults at runtime.

>[!IMPORTANT]
> The `managed-agents-2026-04-01` beta header is set automatically by the wrapper when any `Client.Vaults.*` route is called.

<br>

## Create a vault

`TVaultCreateParams` exposes exactly two fields: `DisplayName` and `Metadata`. There is **no** `Name` and **no** `Description` field — descriptive text belongs in metadata.

```pascal
// uses Anthropic, Anthropic.Types, Anthropic.Helpers,
//      Anthropic.Vaults, Anthropic.Async.Promise;
// Client: IAnthropic;

var Payload: TVaultCreateParamProc :=
  procedure (Params: TVaultCreateParams)
  begin
    Params
      .DisplayName('Linear MCP — team-platform')
      .Metadata('owner', 'team-platform')
      .Metadata('purpose', 'mcp-server-credentials');
  end;

// Asynchronous (promise-based)
var Promise := Client.Vaults.AsyncAwaitCreate(Payload);
Promise
  .&Then(
    procedure (Value: TVault)
    begin
      Display(TutorialHub, Value);
    end)
  .&Catch(
    procedure (E: Exception)
    begin
      Display(TutorialHub, E.Message);
    end);

// Synchronous
//  var Value := Client.Vaults.Create(Payload);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

After creation, `Value.Id` is the vault identifier. The `Metadata` property on the returned object is the raw JSON text of the persisted metadata object.

<br>

## Retrieve / list vaults

`TVaultListParams` exposes `IncludeArchived`, `Limit`, and `Page` (cursor pagination). `Limit` defaults to 20 and is capped server-side.

```pascal
// Single vault
var Vault := Client.Vaults.Retrieve('vault_01ABCxyz');
try
  Display(TutorialHub, Vault);
finally
  Vault.Free;
end;

// Unfiltered list
var All := Client.Vaults.List;

// Filtered list with pagination
var Page1 := Client.Vaults.List(
  procedure (Params: TVaultListParams)
  begin
    Params
      .IncludeArchived(False)
      .Limit(50);
  end);
try
  Display(TutorialHub, Page1);

  if Page1.NextPage <> '' then
    begin
      var Page2 := Client.Vaults.List(
        procedure (Params: TVaultListParams)
        begin
          Params.Page(Page1.NextPage).Limit(50);
        end);
      try
        Display(TutorialHub, Page2);
      finally
        Page2.Free;
      end;
    end;
finally
  Page1.Free;
end;
```

<br>

## Update a vault

`Update` is a sparse PATCH that supports `DisplayName` and `Metadata`. Use `DeleteMetadata(Key)` to drop a single metadata key.

```pascal
var Updated := Client.Vaults.Update('vault_01ABCxyz',
  procedure (Params: TVaultUpdateParams)
  begin
    Params
      .DisplayName('Linear MCP — production')
      .Metadata('owner', 'team-platform')   // upsert
      .DeleteMetadata('legacy-tag');        // drop one key
  end);
try
  Display(TutorialHub, Updated);
finally
  Updated.Free;
end;
```

<br>

## Archive / delete a vault

Prefer `Archive` whenever the vault has been referenced by historical sessions: archived vaults remain visible to those sessions but cannot be mounted by new ones. `Delete` is a hard delete.

```pascal
var Archived := Client.Vaults.Archive('vault_01ABCxyz');
try
  // Archived.ArchivedAt is now populated (RFC 3339 timestamp).
  Display(TutorialHub, Archived);
finally
  Archived.Free;
end;

var Deleted := Client.Vaults.Delete('vault_01ABCxyz');
try
  // Deleted.Id, Deleted.&Type = 'vault_deleted'
  Display(TutorialHub, Deleted);
finally
  Deleted.Free;
end;
```

<br>

## Create a credential — static bearer

The simplest credential type wraps a long-lived bearer token used to authenticate against an MCP server. Build the auth block with `Vault.Credential.CreateStaticBearer(Token, MCPServerUrl)`.

```pascal
var Cred := Client.Vaults.Credentials.Create('vault_01ABCxyz',
  procedure (Params: TVaultCredentialCreateParams)
  begin
    with Generation do
      Params
        .Auth(
           Vault.Credential.CreateStaticBearer(
             'mcp_bearer_xxxxxxxxxxxxxxxxxxxx',
             'https://mcp.linear.app/sse') )
        .DisplayName('Linear MCP — read-only bearer')
        .Metadata('scope', 'read-only');
  end);
try
  // Cred.Id, Cred.&Type = 'vault_credential',
  // Cred.Auth.&Type = 'static_bearer', Cred.Auth.MCPServerUrl
  // (secrets are scrubbed from the response)
  Display(TutorialHub, Cred);
finally
  Cred.Free;
end;
```

>[!IMPORTANT]
> The bearer `Token` is **never** returned by the API after creation. Read-back is limited to metadata, `DisplayName`, `Auth.Type`, and `Auth.MCPServerUrl`. Re-issue the credential (or rotate via `Update`) when the secret is lost.

<br>

## Create a credential — MCP OAuth (access token only)

When the access token is provisioned ahead of time and never refreshed by Anthropic, use the OAuth shape without a `Refresh` sub-block.

```pascal
var Cred := Client.Vaults.Credentials.Create('vault_01ABCxyz',
  procedure (Params: TVaultCredentialCreateParams)
  begin
    with Generation do
      Params
        .Auth(
           Vault.Credential.CreateMCPOAuth(
             'mcp_access_xxxxxxxxxxxxxxxxxxxx',
             'https://mcp.linear.app/sse')
             .ExpiresAt('2026-08-01T00:00:00Z') )
        .DisplayName('Linear MCP — OAuth (no refresh)');
  end);
```

`ExpiresAt` is optional but recommended: it lets the server short-circuit calls once the token has expired instead of waiting for the MCP server to reject them.

<br>

## Create a credential — MCP OAuth with refresh

When the MCP server supports refresh-token exchange, attach a `Refresh` sub-block so Anthropic can rotate the access token automatically. The sub-block carries its own `TokenEndpointAuth` (none / basic / post) used during the refresh exchange.

```pascal
var Cred := Client.Vaults.Credentials.Create('vault_01ABCxyz',
  procedure (Params: TVaultCredentialCreateParams)
  begin
    with Generation do
      Params
        .Auth(
           Vault.Credential.CreateMCPOAuth(
             'mcp_access_xxxxxxxxxxxxxxxxxxxx',
             'https://mcp.example.com/sse')
             .ExpiresAt('2026-05-22T18:00:00Z')
             .Refresh(
                Vault.Credential.CreateRefresh
                  .ClientId('my-client-id')
                  .RefreshToken('mcp_refresh_xxxxxxxxxxxxxxxxxxxx')
                  .TokenEndpoint('https://auth.example.com/oauth/token')
                  .TokenEndpointAuth(
                     Vault.Credential.CreateBasicTokenEndpointAuth('client_secret_xxx'))
                  .Scope('mcp.read mcp.write')
                  .Resource('https://mcp.example.com')) )
        .DisplayName('Acme MCP — OAuth with refresh');
  end);
```

The three token-endpoint authentication shapes:

| Helper | Effect at the token endpoint |
|---|---|
| `Vault.Credential.CreateNoTokenEndpointAuth` | Public client — no client secret sent. |
| `Vault.Credential.CreateBasicTokenEndpointAuth(ClientSecret)` | `Authorization: Basic <client_id:client_secret>` header. |
| `Vault.Credential.CreatePostTokenEndpointAuth(ClientSecret)` | `client_id` + `client_secret` in the request body. |

<br>

## Retrieve / list credentials

```pascal
// Single credential
var Cred := Client.Vaults.Credentials.Retrieve('vault_01ABCxyz', 'cred_01ZZZ');
try
  Display(TutorialHub, Cred);
finally
  Cred.Free;
end;

// List with pagination
var Page1 := Client.Vaults.Credentials.List('vault_01ABCxyz',
  procedure (Params: TVaultCredentialListParams)
  begin
    Params
      .IncludeArchived(False)
      .Limit(50);
  end);
try
  for var C in Page1.Data do
    begin
      // Discriminate on the persisted auth shape
      if Assigned(C.Auth) and C.Auth.IsClientSecretBasic then
        Display(TutorialHub, C);

      // Or simply rely on Auth.&Type ('mcp_oauth' / 'static_bearer')
      Display(TutorialHub, C);
    end;

  if Page1.NextPage <> '' then
    begin
      var Page2 := Client.Vaults.Credentials.List('vault_01ABCxyz',
        procedure (Params: TVaultCredentialListParams)
        begin
          Params.Page(Page1.NextPage).Limit(50);
        end);
      try
        Display(TutorialHub, Page2);
      finally
        Page2.Free;
      end;
    end;
finally
  Page1.Free;
end;
```

`TVaultCredentialListParams` exposes `IncludeArchived`, `Limit`, and `Page`.

<br>

## Update a credential (rotate secret, edit metadata)

`Update` accepts a fresh `Auth(...)` block (same two shapes — `UpdateStaticBearer` / `UpdateMCPOAuth`), `DisplayName`, `Metadata` (upsert + overload), and `DeleteMetadata`. This is the route to use when a token leaks and you need to rotate the secret without re-issuing the credential id.

```pascal
// Rotate a static bearer token, keep everything else
var Updated := Client.Vaults.Credentials.Update('vault_01ABCxyz', 'cred_01ZZZ',
  procedure (Params: TVaultCredentialUpdateParams)
  begin
    with Generation do
      Params
        .Auth(
           Vault.Credential.UpdateStaticBearer
             .Token('mcp_bearer_NEW_xxxxxxxxxxxxxxxx') )
        .Metadata('rotated-at', '2026-05-15T10:00:00Z');
  end);
```

```pascal
// Rotate an OAuth access token AND the refresh token in one shot
var Updated := Client.Vaults.Credentials.Update('vault_01ABCxyz', 'cred_01ZZZ',
  procedure (Params: TVaultCredentialUpdateParams)
  begin
    with Generation do
      Params
        .Auth(
           Vault.Credential.UpdateMCPOAuth
             .AccessToken('mcp_access_NEW_xxxxxxxxxxxxxxxx')
             .ExpiresAt('2026-08-15T00:00:00Z')
             .Refresh(
                Vault.Credential.UpdateRefresh
                  .RefreshToken('mcp_refresh_NEW_xxxxxxxxxxxxxxxx')
                  .Scope('mcp.read mcp.write mcp.admin')) )
        .DisplayName('Acme MCP — OAuth (rotated 2026-05-15)');
  end);
```

>[!IMPORTANT]
> The update auth helpers (`UpdateMCPOAuth`, `UpdateStaticBearer`, `UpdateRefresh`) accept only the fields you want to change — omitted fields preserve the server-side values. Omitting the `Auth(...)` setter altogether leaves the credential secret completely untouched.

<br>

## Archive / delete a credential

```pascal
// Archive — visible to historical sessions, hidden from new ones
var Archived := Client.Vaults.Credentials.Archive('vault_01ABCxyz', 'cred_01ZZZ');
try
  Display(TutorialHub, Archived);
finally
  Archived.Free;
end;

// Hard delete
var Deleted := Client.Vaults.Credentials.Delete('vault_01ABCxyz', 'cred_01ZZZ');
try
  // Deleted.Id, Deleted.&Type = 'vault_credential_deleted'
  Display(TutorialHub, Deleted);
finally
  Deleted.Free;
end;
```

<br>

## Validate a credential

`Validate` exercises the credential end-to-end: it issues a probe MCP request, optionally performs a refresh-token exchange, and reports what the upstream server replied. The secret value is never echoed back — only the diagnostic envelope.

```pascal
var Status := Client.Vaults.Credentials.Validate('vault_01ABCxyz', 'cred_01ZZZ');
try
  // Top-level status
  Display(TutorialHub, Status);

  // Drill into the MCP probe response
  if Assigned(Status.MCPProbe) and Assigned(Status.MCPProbe.HTTPResponse) then
    begin
      Display(TutorialHub, F('mcp_probe.method',      Status.MCPProbe.Method));
      Display(TutorialHub, F('mcp_probe.status_code', Status.MCPProbe.HTTPResponse.StatusCode.ToString));
      Display(TutorialHub, F('mcp_probe.content_type',Status.MCPProbe.HTTPResponse.ContentType));
      Display(TutorialHub, F('mcp_probe.body',        Status.MCPProbe.HTTPResponse.Body));
    end;

  // For OAuth credentials with a refresh block, the exchange result is reported here
  if Status.HasRefreshToken and Assigned(Status.Refresh) then
    Display(TutorialHub, F('refresh.status', Status.Refresh.Status));
finally
  Status.Free;
end;
```

Returned fields on `TVaultCredentialValidation`:

| Field | Meaning |
|---|---|
| `Status` | Overall validation status (`valid`, `invalid`, …). |
| `CredentialId` / `VaultId` | Identifiers of the credential under test. |
| `ValidatedAt` | RFC 3339 timestamp of the probe. |
| `HasRefreshToken` | `True` when an MCP-OAuth refresh block is configured. |
| `MCPProbe` | HTTP response captured by the upstream MCP server (status code, content-type, scrubbed body). |
| `Refresh` | Result of the refresh-token exchange, when applicable (`Status`, plus HTTP response details). |

<br>

## Attaching vaults to a session

A session declares the vaults it can pull credentials from via `VaultIds([...])`. The server consults each listed vault when a tool or MCP server needs a secret.

```pascal
var Session := Client.Sessions.Create(
  procedure (Params: TSessionCreateParams)
  begin
    Params
      .Agent('agent_01ABCxyz')
      .EnvironmentId('env_01ABCxyz')
      .Title('Linear triage run')
      .VaultIds(['vault_01LINEAR', 'vault_01GITHUB']);
  end);
```

The same setter exists on `TSessionUpdateParams`, so you can rotate the attached vault list on an already-running session.

<br>

## Webhook events

Vault and credential lifecycle changes (create, update, archive, delete, validation, refresh) emit webhook events. The wrapper exposes two predicates on the dispatched event data:

- `TWebhookEventData.IsVaultEvent` — vault lifecycle (create / update / archive / delete).
- `TWebhookEventData.IsVaultCredentialEvent` — credential lifecycle (including validation outcomes).

See the [Webhooks guide](webhooks.md) for the full event taxonomy and the dispatcher API.

<br>

## Constraints & notes

- Credential **secrets** (`Token`, `AccessToken`, `RefreshToken`, `ClientSecret`) are never returned by the API after creation. Read-back is limited to identifiers, `DisplayName`, `Auth.Type`, `Auth.MCPServerUrl`, metadata, and timestamps.
- The two `Auth` shapes are mutually exclusive: a single credential is either `static_bearer` **or** `mcp_oauth`, never both.
- `TVaultCreateParams` / `TVaultUpdateParams` expose `DisplayName` only — there is no separate `Description` field, store descriptive text in metadata instead.
- `Update` is sparse: omitting `Auth(...)` leaves the persisted secret untouched. To rotate only the access token of an MCP-OAuth credential, pass `Vault.Credential.UpdateMCPOAuth.AccessToken(NewValue)` and nothing else.
- Archive instead of delete whenever the credential has been referenced by historical sessions; archived credentials remain visible to those sessions but cannot be mounted by new ones.
- The `managed-agents-2026-04-01` beta header is set automatically by the wrapper.

<br>

## References

- Source unit: [`Anthropic.Vaults.pas`](../source/Anthropic.Vaults.pas)
- Helper unit: [`Anthropic.Helpers.pas`](../source/Anthropic.Helpers.pas) (`Generation.Vault` — `CreateParams`, `UpdateParams`, `ListParams`; `Generation.Vault.Credential` — `CreateParams`, `UpdateParams`, `ListParams`, `CreateMCPOAuth`, `CreateStaticBearer`, `CreateRefresh`, `CreateNoTokenEndpointAuth`, `CreateBasicTokenEndpointAuth`, `CreatePostTokenEndpointAuth`, `UpdateMCPOAuth`, `UpdateStaticBearer`, `UpdateRefresh`, `UpdateBasicTokenEndpointAuth`, `UpdatePostTokenEndpointAuth`)
- Companion guides: [Managed Agents overview](managed-agents.md), [Sessions](managed-agents-sessions.md), [Webhooks](webhooks.md)
