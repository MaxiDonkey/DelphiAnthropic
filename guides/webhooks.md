# Webhooks

**Webhooks** are the asynchronous notification channel for the Managed Agents API. Anthropic delivers signed HTTP callbacks when a managed resource changes state (session, session thread, vault, vault credential). The wrapper ships a self-contained HMAC verifier, a stateful receiver, and a strongly typed event surface.

- [Overview](#overview)
- [Required HTTP headers](#required-http-headers)
- [Verify a delivery (Boolean form)](#verify-a-delivery-boolean-form)
- [Verify a delivery (exception form)](#verify-a-delivery-exception-form)
- [Unwrap a delivery in one step](#unwrap-a-delivery-in-one-step)
- [Stateful receiver](#stateful-receiver)
- [Inspect the parsed event](#inspect-the-parsed-event)
- [Switch on the event type](#switch-on-the-event-type)
- [Forward-compatible parsing](#forward-compatible-parsing)
- [Supported event types](#supported-event-types)
- [Exception hierarchy](#exception-hierarchy)
- [Constraints & notes](#constraints--notes)
- [References](#references)

___

<br>

## Overview

Two layers are exposed:

| Layer | API | Purpose |
|---|---|---|
| Verification (stateless) | `TWebhookVerifier` (record, class methods) | HMAC-SHA256 signature check with constant-time comparison and freshness window. |
| Verification (stateful) | `TWebhookReceiver` (class) | Same primitives, bound to a signing key + tolerance once. |
| Event surface | `TWebhookEvent` / `TWebhookEventData` (`TJSONFingerprint` classes) | Strongly typed parsing with `TWebhookEventType` / `TWebhookResourceKind`. |

Both verifier classes are declared in [`Anthropic.Webhooks.pas`](../source/Anthropic.Webhooks.pas). `TWebhookVerifier` is a `record` with `class` methods, so it does not need to be instantiated.

<br>

## Required HTTP headers

Every delivery carries three headers. The wrapper accepts both the canonical names and the `x-` prefixed variants — case-insensitive on either form.

| Canonical name | Alias | Content |
|---|---|---|
| `webhook-id` | `x-webhook-id` | Opaque delivery identifier. |
| `webhook-timestamp` | `x-webhook-timestamp` | Unix epoch seconds when the delivery was signed. |
| `webhook-signature` | `x-webhook-signature` | Space-separated list of `v1,<base64>` fragments. The wrapper accepts the first fragment whose HMAC matches. |

You hand all three over to the verifier through a `System.Net.URLClient.TNetHeaders` value:

```pascal
// uses System.Net.URLClient, Anthropic.Webhooks;

var Headers: TNetHeaders;
SetLength(Headers, 3);
Headers[0] := TNameValuePair.Create('webhook-id',        Request.GetHeader('webhook-id'));
Headers[1] := TNameValuePair.Create('webhook-timestamp', Request.GetHeader('webhook-timestamp'));
Headers[2] := TNameValuePair.Create('webhook-signature', Request.GetHeader('webhook-signature'));
```

`Request.GetHeader` here stands in for whatever HTTP server you are using (Indy, mORMot, WebBroker, …) — the wrapper does not care about the source, only the final `TNetHeaders` shape.

<br>

## Verify a delivery (Boolean form)

`Verify` returns `True` when the signature matches **and** the timestamp is within the freshness window. The signing key registered for the endpoint must start with `whsec_`.

```pascal
// uses Anthropic.Webhooks;

const
  SigningKey = 'whsec_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx';

if TWebhookVerifier.Verify(PayloadString, RequestHeaders, SigningKey) then
  ProcessDelivery(PayloadString)
else
  RejectDelivery;
```

Both `Verify` and `VerifyOrRaise` accept either a `string` payload (UTF-8 encoded internally) or a raw `TBytes` payload. Use the `TBytes` overload when you have access to the original request body bytes — it skips the round-trip through `TEncoding.UTF8` and removes one source of encoding drift.

```pascal
// Raw bytes overload — preferred for HTTP servers that surface the body as TBytes
if TWebhookVerifier.Verify(PayloadBytes, RequestHeaders, SigningKey) then
  ProcessDelivery(PayloadBytes);
```

The freshness window defaults to `TWebhookVerifier.DefaultToleranceSeconds` (300 s). Override it with the last argument when your environment needs a tighter or looser bound:

```pascal
// 60-second freshness window
if TWebhookVerifier.Verify(PayloadString, RequestHeaders, SigningKey, 60) then
  ProcessDelivery(PayloadString);
```

Passing `MaxAgeSeconds <= 0` disables the timestamp check entirely. Only do this when replaying captured deliveries offline — in production it removes a defense against replay attacks.

<br>

## Verify a delivery (exception form)

When you need to distinguish *why* a delivery was rejected (missing header vs. bad signature vs. stale timestamp), call `VerifyOrRaise` and catch the specific exception classes:

```pascal
try
  TWebhookVerifier.VerifyOrRaise(PayloadString, RequestHeaders, SigningKey);
  ProcessDelivery(PayloadString);
except
  on E: EAnthropicWebhookMissingHeader do
    // 400 — the request is missing webhook-id / -timestamp / -signature
    Reply400(E.Message);
  on E: EAnthropicWebhookMissingSigningKey do
    // 500 — the endpoint is misconfigured (key empty)
    Reply500('Webhook signing key not configured');
  on E: EAnthropicWebhookInvalidSignature do
    // 401 — HMAC mismatch (do NOT echo E.Message in the response body)
    Reply401;
  on E: EAnthropicWebhookStalePayload do
    // 408 — outside the freshness window; the sender will redeliver
    Reply408;
end;
```

`VerifyOrRaise` accepts the same `string` / `TBytes` overloads as `Verify`.

<br>

## Unwrap a delivery in one step

`Unwrap` chains `VerifyOrRaise` + JSON parse and returns a typed `TWebhookEvent`. Use it when you don't need to keep the raw payload after parsing.

```pascal
var Event := TWebhookVerifier.Unwrap(PayloadString, RequestHeaders, SigningKey);
try
  // Event.Id          — opaque delivery identifier (matches the webhook-id header)
  // Event.CreatedAt   — RFC 3339 timestamp set by the sender
  // Event.&Type       — wire-form event type, e.g. 'session.requires_action'
  // Event.Data        — TWebhookEventData with ids and predicates
  HandleEvent(Event);
finally
  Event.Free;
end;
```

On any verification failure, `Unwrap` raises the same exceptions as `VerifyOrRaise`. On a malformed body, it raises `EAnthropicWebhookInvalidPayload`.

<br>

## Stateful receiver

For endpoints that handle many deliveries, `TWebhookReceiver` binds the signing key and tolerance once. It is a thin instance wrapper around `TWebhookVerifier` — every call delegates to the static API.

```pascal
// uses Anthropic.Webhooks;

// Construct once at startup
var Receiver := TWebhookReceiver.Create('whsec_xxxxxxxxxxxxxxxxxxxx', 120);  // 120s tolerance
try
  // Per-request: same surface as TWebhookVerifier, without re-passing the key
  if Receiver.Verify(PayloadString, RequestHeaders) then
    ProcessDelivery(PayloadString);

  var Event := Receiver.Unwrap(PayloadString, RequestHeaders);
  try
    HandleEvent(Event);
  finally
    Event.Free;
  end;
finally
  Receiver.Free;
end;
```

Both `SigningKey` and `MaxAgeSeconds` are read/write properties — handy for rotating the secret without rebuilding the receiver:

```pascal
Receiver.SigningKey   := NewSigningKey;
Receiver.MaxAgeSeconds := 60;
```

<br>

## Inspect the parsed event

`Event.Data` exposes the identifiers carried by the delivery and four convenience predicates that bucket the event by parent resource. The wrapper ships `Display` overloads for both `TWebhookEvent` and `TWebhookEventData` — the event-level overload also renders its nested `Data` block.

```pascal
var Event := TWebhookVerifier.Unwrap(PayloadString, RequestHeaders, SigningKey);
try
  // Single call: dumps Event.Id / Type / CreatedAt, then Data with its ids and resolved ResourceKind
  Display(TutorialHub, Event);

  // VaultId is populated for vault_credential.* deliveries
  if Event.Data.IsVaultCredentialEvent then
    Display(TutorialHub, F('parent vault id', Event.Data.VaultId));

  // Bucket by resource without enumerating every event type
  if Event.Data.IsSessionEvent         then HandleSession      (Event);
  if Event.Data.IsSessionThreadEvent   then HandleSessionThread(Event);
  if Event.Data.IsVaultEvent           then HandleVault        (Event);
  if Event.Data.IsVaultCredentialEvent then HandleCredential   (Event);
finally
  Event.Free;
end;
```

The four predicates are derived from `ResourceKind`, which itself maps every known event type to one of `session`, `session_thread`, `vault`, `vault_credential`, or `unknown` for forward-compatible payloads.

To inspect only the data envelope (without re-dumping the wrapper fields), call `Display(TutorialHub, Event.Data)` — useful when you want to log just the resource identifiers.

<br>

## Switch on the event type

`Event.Data.EventType` returns the strongly typed enum value. It **raises** `EAnthropicWebhookInvalidPayload` on an unknown type — use `TryGetEventType` (next section) when you want a forward-compatible reader.

```pascal
case Event.Data.EventType of
  TWebhookEventType.session_created:
    EnqueueWelcomeMessage(Event.Data.Id);

  TWebhookEventType.session_requires_action:
    // The agent is waiting for a tool confirmation or custom-tool result.
    // Resume by sending the appropriate event via Client.Sessions.Events.Send.
    PromptUserForApproval(Event.Data.Id);

  TWebhookEventType.session_status_terminated,
  TWebhookEventType.session_archived,
  TWebhookEventType.session_deleted:
    CloseSessionUI(Event.Data.Id);

  TWebhookEventType.vault_credential_refresh_failed:
    // The MCP-OAuth refresh exchange failed — page the on-call owner.
    PageOnCall(
      'Credential ' + Event.Data.Id +
      ' in vault ' + Event.Data.VaultId +
      ' failed to refresh');
end;
```

<br>

## Forward-compatible parsing

When Anthropic adds new event types, deliveries you don't yet recognize will still arrive. Two API shapes make this safe:

```pascal
// 1) TryGetEventType on the event data — returns False instead of raising
var Kind: TWebhookEventType;
if Event.Data.TryGetEventType(Kind) then
  HandleKnown(Kind)
else
  LogUnknownEvent(Event.&Type);   // fall back on the raw wire-form string

// 2) Top-level enum helper
var Parsed: TWebhookEventType;
if TWebhookEventType.TryParse(Event.&Type, Parsed) then
  HandleKnown(Parsed)
else
  LogUnknownEvent(Event.&Type);

// 3) ResourceKind never raises — returns TWebhookResourceKind.unknown on an unknown type
case Event.Data.ResourceKind of
  TWebhookResourceKind.session:          HandleSession(Event);
  TWebhookResourceKind.session_thread:   HandleSessionThread(Event);
  TWebhookResourceKind.vault:            HandleVault(Event);
  TWebhookResourceKind.vault_credential: HandleCredential(Event);
  TWebhookResourceKind.unknown:          LogUnknownEvent(Event.&Type);
end;
```

Prefer one of these shapes for production handlers — calling `Event.Data.EventType` directly is safe only when you can afford to drop unknown deliveries with a 4xx.

<br>

## Supported event types

Defined in [`Anthropic.Types.pas`](../source/Anthropic.Types.pas) (`TWebhookEventType`).

### Session (`ResourceKind = session`)

| Enum value | Wire-form type string |
|---|---|
| `session_archived` | `session.archived` |
| `session_created` | `session.created` |
| `session_deleted` | `session.deleted` |
| `session_idled` | `session.idled` |
| `session_outcome_evaluation_ended` | `session.outcome_evaluation_ended` |
| `session_pending` | `session.pending` |
| `session_requires_action` | `session.requires_action` |
| `session_running` | `session.running` |
| `session_status_idled` | `session.status_idled` |
| `session_status_rescheduled` | `session.status_rescheduled` |
| `session_status_run_started` | `session.status_run_started` |
| `session_status_terminated` | `session.status_terminated` |

### Session thread (`ResourceKind = session_thread`)

| Enum value | Wire-form type string |
|---|---|
| `session_thread_created` | `session.thread_created` |
| `session_thread_idled` | `session.thread_idled` |
| `session_thread_terminated` | `session.thread_terminated` |

### Vault (`ResourceKind = vault`)

| Enum value | Wire-form type string |
|---|---|
| `vault_archived` | `vault.archived` |
| `vault_created` | `vault.created` |
| `vault_deleted` | `vault.deleted` |

### Vault credential (`ResourceKind = vault_credential`)

| Enum value | Wire-form type string |
|---|---|
| `vault_credential_archived` | `vault_credential.archived` |
| `vault_credential_created` | `vault_credential.created` |
| `vault_credential_deleted` | `vault_credential.deleted` |
| `vault_credential_refresh_failed` | `vault_credential.refresh_failed` |

<br>

## Exception hierarchy

All webhook-specific exceptions descend from `EAnthropicWebhookException`. Catch the parent class for a coarse-grained handler, or the concrete subclasses for precise responses.

| Exception | Cause | Suggested HTTP response |
|---|---|---|
| `EAnthropicWebhookMissingSigningKey` | The signing key argument was empty (or only contained the `whsec_` prefix). | 500 — endpoint misconfiguration. |
| `EAnthropicWebhookMissingHeader` | One of `webhook-id` / `webhook-timestamp` / `webhook-signature` is absent. | 400 — bad request. |
| `EAnthropicWebhookInvalidSignature` | The signing key is not valid base64, **or** no signature fragment matched the computed HMAC, **or** the timestamp could not be parsed. | 401 — do not echo `E.Message` to untrusted callers. |
| `EAnthropicWebhookStalePayload` | The delivery timestamp exceeded the freshness window. | 408 — the sender will redeliver. |
| `EAnthropicWebhookInvalidPayload` | The body could not be parsed as JSON, is missing the `data` envelope, or carries an event type the wrapper does not recognize (only from `EventType`, not from `TryGetEventType`). | 422 — unprocessable entity. |

<br>

## Constraints & notes

- Always verify the signature **before** parsing or acting on the payload. `Unwrap` enforces this ordering for you.
- Compare signatures with the wrapper's `Verify` / `VerifyOrRaise`; do not re-implement HMAC comparison ad-hoc — the wrapper uses a constant-time comparator (`TWebhookVerifier.ConstantTimeEquals`).
- The default 300 s freshness window matches the typical Anthropic redelivery policy; widen it only when offline-replaying captured deliveries, and never disable it in production.
- The signature header may carry multiple `v1,<base64>` fragments (key rotation). The verifier accepts the delivery if **any** fragment matches — pass the full header value untouched.
- Webhook deliveries are independent from the synchronous `StreamRaw` event channel — use webhooks when you need to be notified without keeping an open SSE connection.

<br>

## References

- Source unit: [`Anthropic.Webhooks.pas`](../source/Anthropic.Webhooks.pas) (`TWebhookVerifier`, `TWebhookReceiver`, `TWebhookEvent`, `TWebhookEventData`, exception classes)
- Event enums: [`Anthropic.Types.pas`](../source/Anthropic.Types.pas) (`TWebhookEventType`, `TWebhookResourceKind` — both `Scoped`; helpers expose `Parse` / `TryParse` / `ToString` / `ResourceKind`)
- Companion guides: [Managed Agents overview](managed-agents.md), [Sessions](managed-agents-sessions.md), [Vaults](managed-agents-vaults.md)
