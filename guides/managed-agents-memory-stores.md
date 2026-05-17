# Memory Stores [beta]

**Memory Stores** provide cross-session persistent memory for managed agents. A Memory Store owns a set of **Memories** (the actual content, addressed by path) and a set of **Memory Versions** (the immutable history of each memory, with redaction support).

- [Overview](#overview)
- [Create a memory store](#create-a-memory-store)
- [Retrieve / list memory stores](#retrieve--list-memory-stores)
- [Update a memory store](#update-a-memory-store)
- [Archive / delete a memory store](#archive--delete-a-memory-store)
- [Create a memory](#create-a-memory)
- [Retrieve a memory (basic vs full view)](#retrieve-a-memory-basic-vs-full-view)
- [List memories (prefix, depth, ordering)](#list-memories-prefix-depth-ordering)
- [Update a memory with optimistic concurrency](#update-a-memory-with-optimistic-concurrency)
- [Delete a memory](#delete-a-memory)
- [Memory versions — list](#memory-versions--list)
- [Memory versions — retrieve](#memory-versions--retrieve)
- [Memory versions — redact](#memory-versions--redact)
- [Attaching a memory store to a session](#attaching-a-memory-store-to-a-session)
- [Constraints & notes](#constraints--notes)
- [References](#references)

___

<br>

## Overview

The wrapper exposes three routes hanging off `Client.MemoryStores`:

| Sub-route | Wrapper accessor | Purpose |
|---|---|---|
| Memory Stores | `Client.MemoryStores` | CRUD + archive on stores |
| Memories | `Client.MemoryStores.Memories` | CRUD on memory entries within a store |
| Memory Versions | `Client.MemoryStores.MemoryVersions` | History and redaction of individual memories |

All operations have both a blocking variant and a promise-based variant (`AsyncAwait*`). Two construction styles are available — direct fluent (`TXxxParams.New.Field(...)`) and via the `Generation.MemoryStore` helper exposed by `Anthropic.Helpers`. The examples below use whichever style is most readable for each case.

>[!IMPORTANT]
> The `managed-agents-2026-04-01` beta header is set automatically by the wrapper when any `Client.MemoryStores.*` route is called.

<br>

## Create a memory store

The minimum required field is `Name`. Description and metadata are optional.

```pascal
// uses Anthropic, Anthropic.Types, Anthropic.Helpers,
//      Anthropic.MemoryStore, Anthropic.Async.Promise;
// Client: IAnthropic;

var Payload: TMemoryStoreCreateParamProc :=
  procedure (Params: TMemoryStoreCreateParams)
  begin
    Params
      .Name('Support agent long-term memory')
      .Description('Cross-session notes for the customer-support agent.')
      .Metadata('owner', 'team-support')
      .Metadata('tier', 'production');
  end;

// Asynchronous (promise-based)
var Promise := Client.MemoryStores.AsyncAwaitCreate(Payload);
Promise
  .&Then(
    procedure (Value: TMemoryStore)
    begin
      Display(TutorialHub, Value);
    end)
  .&Catch(
    procedure (E: Exception)
    begin
      Display(TutorialHub, E.Message);
    end);

// Synchronous
//  var Value := Client.MemoryStores.Create(Payload);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

After creation, `Value.Id` is the store identifier — use it for `Retrieve` / `Update` / `Archive` / `Delete` and to address `Memories` / `MemoryVersions`.

<br>

## Retrieve / list memory stores

```pascal
// Single store
var Store := Client.MemoryStores.Retrieve('mems_01ABCxyz');
try
  Display(TutorialHub, Store);
finally
  Store.Free;
end;

// Unfiltered list
var All := Client.MemoryStores.List;

// Filtered list with cursor pagination
var Page1 := Client.MemoryStores.List(
  procedure (Params: TMemoryStoreListParams)
  begin
    Params
      .CreatedAtGte('2026-01-01T00:00:00Z')
      .IncludeArchived(False)
      .Limit(50);
  end);
try
  Display(TutorialHub, Page1);

  if Page1.NextPage <> '' then
    begin
      var Page2 := Client.MemoryStores.List(
        procedure (Params: TMemoryStoreListParams)
        begin
          Params
            .Limit(50)
            .Page(Page1.NextPage);
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

`TMemoryStoreListParams` supports `CreatedAtGte`, `CreatedAtLte`, `IncludeArchived`, `Limit`, and `Page`. `Limit` defaults to 20 and is capped at 100.

<br>

## Update a memory store

`Update` is a sparse PATCH: omit fields you want to leave untouched, use `DeleteMetadata(Key)` to drop a single metadata key.

```pascal
var Updated := Client.MemoryStores.Update('mems_01ABCxyz',
  procedure (Params: TMemoryStoreUpdateParams)
  begin
    Params
      .Description('Now also used by the triage coordinator.')
      .Metadata('owner', 'team-platform')   // upsert
      .DeleteMetadata('cost-center');       // drop a single key
  end);
try
  Display(TutorialHub, Updated);
finally
  Updated.Free;
end;
```

<br>

## Archive / delete a memory store

Prefer `Archive` whenever the store has been referenced by historical sessions: archived stores remain visible to those sessions but cannot be mounted by new ones. `Delete` is a hard delete.

```pascal
var Archived := Client.MemoryStores.Archive('mems_01ABCxyz');
try
  // Archived.ArchivedAt is now populated (RFC 3339 timestamp).
  Display(TutorialHub, Archived);
finally
  Archived.Free;
end;

var Deleted := Client.MemoryStores.Delete('mems_01ABCxyz');
try
  // Deleted.Id, Deleted.&Type = 'memory_store_deleted'
  Display(TutorialHub, Deleted);
finally
  Deleted.Free;
end;
```

<br>

## Create a memory

A memory is addressed by a **path** (a string used as the logical key inside the store). The wrapper does not provide a shortcut string overload — every create goes through the parameter procedure with `Path(...)` and `Content(...)`.

```pascal
var Created := Client.MemoryStores.Memories.Create('mems_01ABCxyz',
  procedure (Params: TMemoryCreateParams)
  begin
    Params
      .Path('/customers/acme-corp/preferences')
      .Content(
        'Prefers email over phone. ' +
        'Tier-1 customer since 2024-09. ' +
        'Primary contact: alice@acme.example.');
  end);
try
  // Created.Id, Created.Path, Created.ContentSHA256, Created.MemoryVersionId
  Display(TutorialHub, Created);
finally
  Created.Free;
end;
```

To return the full representation in one round-trip (including the persisted `Content`), use the overload that takes a view query:

```pascal
var Created := Client.MemoryStores.Memories.Create('mems_01ABCxyz',
  procedure (Params: TMemoryCreateParams)
  begin
    Params
      .Path('/customers/acme-corp/preferences')
      .Content('Prefers email over phone.');
  end,
  procedure (Query: TMemoryViewParams)
  begin
    Query.Full;     // equivalent to Query.View('full')
  end);
```

Without the query, the API defaults to the basic view: `Content` is empty on the returned object even though it has been persisted server-side.

<br>

## Retrieve a memory (basic vs full view)

```pascal
// Basic view — metadata only (Content is empty)
var Mem := Client.MemoryStores.Memories.Retrieve('mems_01ABCxyz', 'mem_01ZZZ');
try
  Display(TutorialHub, Mem);
finally
  Mem.Free;
end;

// Full view — includes the UTF-8 Content
var WithBody := Client.MemoryStores.Memories.Retrieve('mems_01ABCxyz', 'mem_01ZZZ',
  procedure (Query: TMemoryViewParams)
  begin
    Query.Full;
  end);
try
  // WithBody.Content is now populated
  Display(TutorialHub, WithBody);
finally
  WithBody.Free;
end;
```

`TMemoryViewParams` exposes three setters: `View(string)`, `Basic`, and `Full`. The helper `Generation.MemoryStore.ViewFull` / `ViewBasic` returns a pre-set instance when you prefer the helper style.

<br>

## List memories (prefix, depth, ordering)

`TMemoryListParams` exposes the rolled-up directory-style listing used by the API. Results contain a mix of concrete **memories** and **prefix markers** (rolled-up parent paths) — the wrapper exposes `IsMemory` / `IsMemoryPrefix` on each `TMemoryListItem` to discriminate.

```pascal
var List := Client.MemoryStores.Memories.List('mems_01ABCxyz',
  procedure (Params: TMemoryListParams)
  begin
    Params
      .PathPrefix('/customers/')   // restrict to this subtree
      .Depth(2)                    // collapse paths deeper than 2 levels
      .OrderBy('path')             // valid values: 'path' or 'created_at'
      .Asc                         // shorthand for .Order('asc')
      .Full                        // include Content on each returned memory
      .Limit(50);
  end);
try
  for var Item in List.Data do
    begin
      if Item.IsMemoryPrefix then
        // Rolled-up prefix — drill deeper with a more specific PathPrefix
        Display(TutorialHub, Item)
      else
      if Item.IsMemory then
        // Concrete memory — Item is a TMemory in disguise
        Display(TutorialHub, TMemory(Item));
    end;

  if List.NextPage <> '' then
    begin
      var Next := Client.MemoryStores.Memories.List('mems_01ABCxyz',
        procedure (Params: TMemoryListParams)
        begin
          Params.Page(List.NextPage).Limit(50);
        end);
      try
        Display(TutorialHub, Next);
      finally
        Next.Free;
      end;
    end;
finally
  List.Free;
end;
```

Available knobs on `TMemoryListParams`: `Depth`, `Limit`, `Order` / `Asc` / `Desc`, `OrderBy`, `Page`, `PathPrefix`, `View` / `Basic` / `Full`.

<br>

## Update a memory with optimistic concurrency

`TMemoryUpdateParams` exposes `Content`, `Path` (to move the memory), and `Precondition` for optimistic concurrency control. Pass the SHA-256 digest of the content you read so the server rejects the call (with `409`) if another writer has changed the memory in the meantime.

```pascal
// Step 1 — fetch the current memory to obtain its ContentSHA256
var Current := Client.MemoryStores.Memories.Retrieve('mems_01ABCxyz', 'mem_01ZZZ',
  procedure (Query: TMemoryViewParams)
  begin
    Query.Full;
  end);
try
  var ExpectedSHA := Current.ContentSHA256;

  // Step 2 — update with a precondition tied to that digest
  var Updated := Client.MemoryStores.Memories.Update('mems_01ABCxyz', 'mem_01ZZZ',
    procedure (Params: TMemoryUpdateParams)
    begin
      with Generation do
        Params
          .Content(
            Current.Content + sLineBreak +
            '2026-05-15: switched primary contact to bob@acme.example.')
          .Precondition( MemoryStore.CreatePrecondition(ExpectedSHA) );
    end);
  try
    Display(TutorialHub, Updated);
  finally
    Updated.Free;
  end;
finally
  Current.Free;
end;
```

`Generation.MemoryStore.CreatePrecondition(SHA)` is equivalent to `TMemoryPreconditionParams.New.ContentSHA256(SHA)`. Omitting `Precondition` performs an unconditional update.

You can also use the overload that takes a view query to receive the full content back in one round-trip:

```pascal
var Updated := Client.MemoryStores.Memories.Update('mems_01ABCxyz', 'mem_01ZZZ',
  procedure (Params: TMemoryUpdateParams)
  begin
    Params.Content('replacement text');
  end,
  procedure (Query: TMemoryViewParams)
  begin
    Query.Full;
  end);
```

<br>

## Delete a memory

`Delete` accepts an optional precondition via `ExpectedContentSHA256`, which serves the same purpose for deletes as `Precondition` does for updates.

```pascal
// Unconditional delete
var Gone := Client.MemoryStores.Memories.Delete('mems_01ABCxyz', 'mem_01ZZZ');
try
  // Gone.Id, Gone.&Type = 'memory_deleted'
  Display(TutorialHub, Gone);
finally
  Gone.Free;
end;

// Delete only if the content still matches the digest we saw
var Safe := Client.MemoryStores.Memories.Delete('mems_01ABCxyz', 'mem_01ZZZ',
  procedure (Params: TMemoryDeleteParams)
  begin
    Params.ExpectedContentSHA256('e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855');
  end);
```

<br>

## Memory versions — list

Every create / update / delete on a memory produces a `TMemoryVersion` entry. The list endpoint exposes rich filtering so you can audit "who wrote what, when, from which session or API key".

```pascal
var Versions := Client.MemoryStores.MemoryVersions.List('mems_01ABCxyz',
  procedure (Params: TMemoryVersionListParams)
  begin
    Params
      .MemoryId('mem_01ZZZ')                    // restrict to one memory
      .CreatedAtGte('2026-05-01T00:00:00Z')
      .Modified                                 // shorthand for .Operation('modified')
      .Full                                     // include content per version
      .Limit(50);
  end);
try
  for var V in Versions.Data do
    // V.Id, V.MemoryId, V.Operation, V.CreatedAt, V.Actor, V.Content, ...
    Display(TutorialHub, V);

  if Versions.NextPage <> '' then
    begin
      var Next := Client.MemoryStores.MemoryVersions.List('mems_01ABCxyz',
        procedure (Params: TMemoryVersionListParams)
        begin
          Params.Page(Versions.NextPage).Limit(50);
        end);
      try
        Display(TutorialHub, Next);
      finally
        Next.Free;
      end;
    end;
finally
  Versions.Free;
end;
```

Available filters on `TMemoryVersionListParams`: `MemoryId`, `SessionId`, `ApiKeyId`, `CreatedAtGte`, `CreatedAtLte`, `Operation` (or shortcuts `Created` / `Modified` / `Deleted`), `View` / `Basic` / `Full`, `Limit`, `Page`.

<br>

## Memory versions — retrieve

```pascal
// Basic view
var V := Client.MemoryStores.MemoryVersions.Retrieve('mems_01ABCxyz', 'memver_01YYY');
try
  Display(TutorialHub, V);
finally
  V.Free;
end;

// Full view (includes the historical Content snapshot)
var VFull := Client.MemoryStores.MemoryVersions.Retrieve('mems_01ABCxyz', 'memver_01YYY',
  procedure (Query: TMemoryViewParams)
  begin
    Query.Full;
  end);
try
  Display(TutorialHub, VFull);
finally
  VFull.Free;
end;
```

The retrieve query parameter procedure is typed as `TMemoryVersionRetrieveParamProc`, which is an alias for `TProc<TMemoryViewParams>` — same `Basic` / `Full` / `View(string)` methods as on memory retrieval.

<br>

## Memory versions — redact

Versions are immutable. To retroactively remove sensitive content from history, **redact** the version: the body is blanked server-side but the version entry (and its actor / timestamp metadata) is preserved for audit.

```pascal
var Redacted := Client.MemoryStores.MemoryVersions.Redact('mems_01ABCxyz', 'memver_01YYY');
try
  // Redacted.Content is now empty; Redacted.RedactedAt is populated.
  Display(TutorialHub, Redacted);
finally
  Redacted.Free;
end;
```

Use redact (not delete) whenever compliance requires keeping the audit trail intact while purging the payload.

<br>

## Attaching a memory store to a session

A memory store becomes useful when mounted as a session resource. See the [Sessions guide](managed-agents-sessions.md) for the full resource model. The relevant params class is `TSessionMemoryStoreResourceParams`, which exposes:

- `MemoryStoreId(Value)` — id of the store to attach.
- `Access(Value)` — `read_write` or `read_only`.
- `Instructions(Value)` — per-attachment guidance shown to the agent.

The helper `Generation.Session.CreateMemoryStoreResource(MemoryStoreId, Access, Instructions)` returns a ready-to-use instance, and `SessionResourcesHelper.AddMemoryStore(...)` adds one straight into a session-creation request.

<br>

## Constraints & notes

- Memories are versioned. To remove sensitive content from history, **redact the relevant versions** — do not rely on `Delete`, which only removes the head and leaves the version chain intact.
- Use `Precondition.ContentSHA256` on updates (and `ExpectedContentSHA256` on deletes) whenever multiple writers may be active. On `409` errors, refetch and retry.
- `Memories.Create` / `Update` / `Retrieve` return a basic view by default; pass a `TMemoryViewParams` (or call `.Full`) when you actually need the `Content` field populated.
- List responses can interleave concrete memories and **prefix markers**. Always test `Item.IsMemory` before casting a `TMemoryListItem` to `TMemory`.
- An archived store cannot be mounted by new sessions. Existing sessions that already reference it continue to work.
- The `managed-agents-2026-04-01` beta header is set automatically by the wrapper.

<br>

## References

- Source unit: [`Anthropic.MemoryStore.pas`](../source/Anthropic.MemoryStore.pas)
- Helper unit: [`Anthropic.Helpers.pas`](../source/Anthropic.Helpers.pas) (`Generation.MemoryStore` — `CreateStoreParams`, `UpdateStoreParams`, `ListStoreParams`, `CreateMemoryParams`, `UpdateMemoryParams`, `ListMemoryParams`, `DeleteMemoryParams`, `CreatePrecondition`, `ListVersionParams`, `ViewBasic`, `ViewFull`)
- Companion guides: [Managed Agents overview](managed-agents.md), [Sessions](managed-agents-sessions.md), [Agents](managed-agents-agents.md), [Environments](managed-agents-environments.md)
