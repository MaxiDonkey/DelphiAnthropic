# Sessions [beta]

A **Session** is a live instance of a managed agent: it binds an `Agent`, an `Environment`, and a set of `Resources`, and exposes its lifecycle through an event stream. Sessions are the unit of work in the Managed Agents API — everything the agent does is observed through session events and thread events.

- [Overview](#overview)
- [Minimal create](#minimal-create)
- [Create with a pinned agent version](#create-with-a-pinned-agent-version)
- [Create with resources (GitHub, files, memory stores)](#create-with-resources-github-files-memory-stores)
- [Create with vaults and metadata](#create-with-vaults-and-metadata)
- [Retrieve / list with filters](#retrieve--list-with-filters)
- [Update](#update)
- [Archive / delete](#archive--delete)
- [List events](#list-events)
- [Send events — user message](#send-events--user-message)
- [Send events — tool confirmation](#send-events--tool-confirmation)
- [Send events — custom tool result](#send-events--custom-tool-result)
- [Send events — interrupt and outcome](#send-events--interrupt-and-outcome)
- [Stream raw session events](#stream-raw-session-events)
- [Add a file resource after the fact](#add-a-file-resource-after-the-fact)
- [List / retrieve / delete resources](#list--retrieve--delete-resources)
- [Rotate a GitHub authorization token](#rotate-a-github-authorization-token)
- [Threads — list and retrieve](#threads--list-and-retrieve)
- [Thread events](#thread-events)
- [Constraints & notes](#constraints--notes)
- [References](#references)

___

<br>

## Overview

`Client.Sessions` exposes the root route plus three nested routes:

| Sub-route | Wrapper accessor | Purpose |
|---|---|---|
| Sessions | `Client.Sessions` | CRUD + archive on sessions |
| Events | `Client.Sessions.Events` | List, send, and stream session-scoped events |
| Resources | `Client.Sessions.Resources` | Attach and manage repository / file / memory-store mounts |
| Threads | `Client.Sessions.Threads` | Enumerate and stream conversation threads |
| Thread Events | `Client.Sessions.Threads.Events` | List and stream events scoped to a single thread |

All operations have both a blocking variant and a promise-based variant (`AsyncAwait*`). The examples below use the `Generation.Session` helper (alias `Session`) along with the `SessionResourceParts`, `SessionContentParts`, and `SessionEventParts` array builders from `Anthropic.Helpers`.

>[!IMPORTANT]
> The `managed-agents-2026-04-01` beta header is set automatically by the wrapper when any `Client.Sessions.*` route is called.

<br>

## Minimal create

A session requires at minimum an agent id and an environment id.

```pascal
// uses Anthropic, Anthropic.Types, Anthropic.Helpers,
//      Anthropic.Sessions, Anthropic.Async.Promise;
// Client: IAnthropic;

var Payload: TSessionCreateParamProc :=
  procedure (Params: TSessionCreateParams)
  begin
    Params
      .Agent('agent_01ABCxyz')            // short form — tracks latest version
      .EnvironmentId('env_01ABCxyz')
      .Title('Release notes for 2026-05 drop');
  end;

// Asynchronous (promise-based)
var Promise := Client.Sessions.AsyncAwaitCreate(Payload);
Promise
  .&Then(
    procedure (Value: TSession)
    begin
      Display(TutorialHub, Value);
    end)
  .&Catch(
    procedure (E: Exception)
    begin
      Display(TutorialHub, E.Message);
    end);

// Synchronous
//  var Value := Client.Sessions.Create(Payload);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

After creation, `Value.Id` is the session identifier. Use it for every subsequent call (`Events`, `Resources`, `Threads`).

<br>

## Create with a pinned agent version

To insulate a long-running session from agent updates, pass an explicit `TSessionAgentParams` carrying both id and version.

```pascal
var Payload: TSessionCreateParamProc :=
  procedure (Params: TSessionCreateParams)
  begin
    with Generation do
      Params
        .Agent( Session.CreateAgent('agent_01ABCxyz', 2) )   // pinned to version 2
        .EnvironmentId('env_01ABCxyz')
        .Title('Triage run — pinned to agent v2');
  end;
```

Passing `Version = 0` (or omitting it) tracks the latest published version of the agent.

<br>

## Create with resources (GitHub, files, memory stores)

Three kinds of resources can be mounted **at session creation**:

- **GitHub repository** — clone a repo with an optional branch / commit checkout, an optional mount path, and an optional authorization token.
- **File** — mount a Files API entry by `file_id` at an optional mount path.
- **Memory store** — attach a memory store with an access mode (`read_write` / `read_only`) and optional per-attachment instructions.

The `SessionResourceParts` array builder bundles all three into a single `Resources([...])` call.

```pascal
var Payload: TSessionCreateParamProc :=
  procedure (Params: TSessionCreateParams)
  begin
    with Generation do
      Params
        .Agent('agent_01ABCxyz')
        .EnvironmentId('env_01ABCxyz')
        .Title('Release notes — repo + style guide')
        .Resources(
           SessionResourceParts
             // 1) GitHub clone with branch checkout and explicit mount path
             .AddResource(
                Session.CreateGitHubRepositoryResource(
                  'https://github.com/MyOrg/twv-browser', 'ghp_xxx')
                  .Checkout( Session.CreateBranchCheckout('main') )
                  .MountPath('/workspace/repo'))
             // 2) File resource (style-guide PDF uploaded via the Files API)
             .AddFile('file_01STYLEGUIDE', '/workspace/docs/style.pdf')
             // 3) Memory store with explicit instructions
             .AddMemoryStore('mems_01SUPPORT', 'read_write',
                'Persist customer-specific preferences keyed by /customers/<slug>.') );
  end);
```

Equivalent without the `Add*` shortcuts (for cases where you need fine-grained control):

```pascal
with Generation do
  Params.Resources([
    Session.CreateGitHubRepositoryResource('https://github.com/MyOrg/twv-browser')
      .Checkout( Session.CreateCommitCheckout('a1b2c3d4e5f6') ),
    Session.CreateFileResource('file_01STYLEGUIDE'),
    Session.CreateMemoryStoreResource('mems_01SUPPORT', 'read_only', '')
  ]);
```

>[!IMPORTANT]
> Only **file** resources can be attached after creation (via `Resources.Add`). GitHub and memory-store resources must be declared at session creation time.

<br>

## Create with vaults and metadata

`VaultIds([...])` mounts one or more credential vaults (see the [Vaults guide](managed-agents-vaults.md)). `Metadata` is a free-form JSON object you can use to tag the session.

```pascal
var Payload: TSessionCreateParamProc :=
  procedure (Params: TSessionCreateParams)
  begin
    Params
      .Agent('agent_01ABCxyz')
      .EnvironmentId('env_01ABCxyz')
      .Title('Linear triage')
      .VaultIds(['vault_01LINEAR', 'vault_01GITHUB'])
      .Metadata('owner', 'team-platform')
      .Metadata('run-kind', 'scheduled');
  end;
```

<br>

## Retrieve / list with filters

```pascal
// Single session
var Session := Client.Sessions.Retrieve('sess_01ABCxyz');
try
  Display(TutorialHub, Session);
finally
  Session.Free;
end;

// Unfiltered list
var All := Client.Sessions.List;

// Filtered list with cursor pagination
var Page1 := Client.Sessions.List(
  procedure (Params: TSessionListParams)
  begin
    Params
      .AgentId('agent_01ABCxyz')
      .AgentVersion(2)                          // restrict to a specific pinned version
      .Statuses(['running', 'paused'])
      .CreatedAtGte('2026-05-01T00:00:00Z')
      .IncludeArchived(False)
      .Order('desc')
      .Limit(50);
  end);
try
  Display(TutorialHub, Page1);

  if Page1.NextPage <> '' then
    begin
      var Page2 := Client.Sessions.List(
        procedure (Params: TSessionListParams)
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

Available filters on `TSessionListParams`: `AgentId`, `AgentVersion`, `CreatedAtGt` / `CreatedAtGte` / `CreatedAtLt` / `CreatedAtLte`, `IncludeArchived`, `Limit`, `MemoryStoreId`, `Order`, `Page`, `Statuses`.

<br>

## Update

`Update` is a sparse PATCH that supports `Title`, `Metadata`, and `VaultIds`. Use `DeleteMetadata(Key)` to drop a single metadata key.

```pascal
var Updated := Client.Sessions.Update('sess_01ABCxyz',
  procedure (Params: TSessionUpdateParams)
  begin
    Params
      .Title('Release notes — final pass')
      .Metadata('reviewed-by', 'alice')
      .DeleteMetadata('draft');
  end);
try
  Display(TutorialHub, Updated);
finally
  Updated.Free;
end;
```

<br>

## Archive / delete

```pascal
var Archived := Client.Sessions.Archive('sess_01ABCxyz');
try
  Display(TutorialHub, Archived);
finally
  Archived.Free;
end;

// Hard delete — only succeeds when the session is no longer active
var Deleted := Client.Sessions.Delete('sess_01ABCxyz');
try
  // Deleted.Id, Deleted.&Type = 'session_deleted'
  Display(TutorialHub, Deleted);
finally
  Deleted.Free;
end;
```

<br>

## List events

`Client.Sessions.Events.List` enumerates the event log of a session. `TSessionEventListParams` supports time-range filters, ordering, page size, cursor pagination, and an event-type filter.

```pascal
var Events := Client.Sessions.Events.List('sess_01ABCxyz',
  procedure (Params: TSessionEventListParams)
  begin
    Params
      .Types(['user_message', 'assistant_message', 'tool_use'])
      .Order('asc')
      .Limit(100);
  end);
try
  for var Evt in Events.Data do
    // Evt.Id, Evt.&Type, Evt.ProcessedAt, Evt.Content, Evt.ToolUseId, Evt.Name, ...
    Display(TutorialHub, Evt);
finally
  Events.Free;
end;
```

The unfiltered overload `Client.Sessions.Events.List('sess_01ABCxyz')` returns the most recent page with default pagination.

<br>

## Send events — user message

The primary way to drive a session forward is to send a `user_message` event. Use the plain text shortcut for simple turns:

```pascal
var Response := Client.Sessions.Events.Send('sess_01ABCxyz',
  procedure (Params: TSessionSendEventsParams)
  begin
    with Generation do
      Params.Events(
        SessionEventParts.AddUserMessage(
          'Draft the release notes from the merged PRs in /workspace/repo.') );
  end);
try
  // Response.Data contains the events the server accepted.
  Display(TutorialHub, Response);
finally
  Response.Free;
end;
```

For rich content (text + images + documents), build a content-block array with `SessionContentParts`:

```pascal
var Response := Client.Sessions.Events.Send('sess_01ABCxyz',
  procedure (Params: TSessionSendEventsParams)
  begin
    with Generation do
      Params.Events(
        SessionEventParts.AddUserMessage(
          SessionContentParts
            .AddText('Apply the tone from this style guide:')
            .AddFileDocument('file_01STYLEGUIDE')
            .AddUrlImage('https://example.com/branding/banner.png')
        ) );
  end);
```

<br>

## Send events — tool confirmation

When a built-in tool is configured with `always_ask` permission, the agent emits a tool-use event and waits for confirmation. Reply with a `tool_confirmation` event carrying the original `ToolUseId`:

```pascal
// Allow the call
var Allow := Client.Sessions.Events.Send('sess_01ABCxyz',
  procedure (Params: TSessionSendEventsParams)
  begin
    with Generation do
      Params.Events(
        SessionEventParts.AddToolConfirmation(
          'toolu_01YYY',                       // ToolUseId from the pending event
          'allow') );                          // valid values: 'allow' or 'deny'
  end);

// Deny the call with an explanation
var Deny := Client.Sessions.Events.Send('sess_01ABCxyz',
  procedure (Params: TSessionSendEventsParams)
  begin
    with Generation do
      Params.Events(
        SessionEventParts.AddToolConfirmation(
          'toolu_01YYY',
          'deny',
          'This path is outside the allowed workspace.') );
  end);
```

The optional fourth argument is `SessionThreadId`, used when the confirmation must be scoped to a specific sub-agent thread.

<br>

## Send events — custom tool result

When the agent is configured with a **custom tool** (executed by the client), the server emits a `custom_tool_use` event and waits for a result. Reply with a `custom_tool_result` carrying the tool's `CustomToolUseId`.

```pascal
var Response := Client.Sessions.Events.Send('sess_01ABCxyz',
  procedure (Params: TSessionSendEventsParams)
  begin
    with Generation do
      Params.Events(
        SessionEventParts.AddCustomToolResult(
          'ctu_01XYZ',                         // CustomToolUseId from the pending event
          SessionContentParts
            .AddText('{"temperature_c":22.3,"conditions":"partly cloudy"}'),
          False                                 // IsError
        ) );
  end);
```

Set the third argument to `True` (and put the diagnostic into the content blocks) when the local tool failed — the agent will see the result as an error and may retry.

<br>

## Send events — interrupt and outcome

```pascal
// Interrupt the agent's current run (optionally scoped to a sub-thread)
var Stop := Client.Sessions.Events.Send('sess_01ABCxyz',
  procedure (Params: TSessionSendEventsParams)
  begin
    with Generation do
      Params.Events(
        SessionEventParts.AddInterrupt() );    // pass a thread id to interrupt only one branch
  end);

// Define a success outcome with a textual rubric
var Outcome := Client.Sessions.Events.Send('sess_01ABCxyz',
  procedure (Params: TSessionSendEventsParams)
  begin
    with Generation do
      Params.Events(
        SessionEventParts.AddOutcome(
          'Produce release notes grouped by area, with PR links.',
          Session.CreateTextRubric(
            'Each section must list at least one PR. ' +
            'No section may exceed 5 bullet points.'),
          10                                     // MaxIterations
        ) );
  end);

// File-backed rubric variant
//   Session.CreateFileRubric('file_01RUBRIC')
```

You can pack several events in a single `Send` call by chaining multiple `Add*` calls on the same `SessionEventParts`.

<br>

## Stream raw session events

`StreamRaw` returns the full SSE payload as a single `TSessionStream`. The wrapper exposes the raw text in `Stream.Data` — parse it line-by-line, or hand it to a higher-level SSE consumer.

```pascal
// Synchronous
var Stream := Client.Sessions.Events.StreamRaw('sess_01ABCxyz');
try
  Display(TutorialHub, Stream.Data);
finally
  Stream.Free;
end;

// Asynchronous variant — useful when wiring the stream to a UI consumer
var Promise := Client.Sessions.Events.AsyncAwaitStreamRaw('sess_01ABCxyz');
Promise
  .&Then(
    procedure (Value: TSessionStream)
    begin
      Display(TutorialHub, Value.Data);
    end)
  .&Catch(
    procedure (E: Exception)
    begin
      Display(TutorialHub, E.Message);
    end);
```

<br>

## Add a file resource after the fact

`Resources.Add` is typed for **file resources only** — it cannot mount a GitHub repository or a memory store post-creation.

```pascal
var Resource := Client.Sessions.Resources.Add('sess_01ABCxyz',
  procedure (Params: TSessionFileResourceParams)
  begin
    Params
      .FileId('file_01EXTRADOC')
      .MountPath('/workspace/docs/extra.pdf');
  end);
try
  // Resource.Id, Resource.Type, Resource.MountPath, ...
  Display(TutorialHub, Resource);
finally
  Resource.Free;
end;
```

If you need to attach a GitHub repo or a memory store to an already-running session, open a new session with the desired resource list — there is no server-side `Add` for those kinds.

<br>

## List / retrieve / delete resources

```pascal
// List with pagination
var Page1 := Client.Sessions.Resources.List('sess_01ABCxyz',
  procedure (Params: TSessionSimpleListParams)
  begin
    Params.Limit(50);
  end);
try
  Display(TutorialHub, Page1);
finally
  Page1.Free;
end;

// Single resource
var R := Client.Sessions.Resources.Retrieve('sess_01ABCxyz', 'res_01YYY');
try
  Display(TutorialHub, R);
finally
  R.Free;
end;

// Remove a resource from the running session
var Deleted := Client.Sessions.Resources.Delete('sess_01ABCxyz', 'res_01YYY');
try
  // Deleted.Id, Deleted.&Type = 'session_resource_deleted'
  Display(TutorialHub, Deleted);
finally
  Deleted.Free;
end;
```

<br>

## Rotate a GitHub authorization token

`Resources.Update` only exposes one mutable field: the authorization token used by a `github_repository` resource. Use it to rotate a token without re-cloning the repo.

```pascal
var Updated := Client.Sessions.Resources.Update('sess_01ABCxyz', 'res_01GIT',
  procedure (Params: TSessionResourceUpdateParams)
  begin
    Params.AuthorizationToken('ghp_new_token_value');
  end);
try
  Display(TutorialHub, Updated);
finally
  Updated.Free;
end;
```

This call is a no-op (and harmless) on non-GitHub resources.

<br>

## Threads — list and retrieve

A thread is one branch of the session's conversation tree (the main thread plus any sub-agent branches).

```pascal
var Threads := Client.Sessions.Threads.List('sess_01ABCxyz',
  procedure (Params: TSessionSimpleListParams)
  begin
    Params.Limit(50);
  end);
try
  for var T in Threads.Data do
    Display(TutorialHub, T);
finally
  Threads.Free;
end;

var Thread := Client.Sessions.Threads.Retrieve('sess_01ABCxyz', 'thd_01ZZZ');
try
  Display(TutorialHub, Thread);
finally
  Thread.Free;
end;

// Raw SSE stream for the full thread
var Stream := Client.Sessions.Threads.StreamRaw('sess_01ABCxyz', 'thd_01ZZZ');
try
  Display(TutorialHub, Stream.Data);
finally
  Stream.Free;
end;
```

<br>

## Thread events

The `Threads.Events` sub-route is the per-thread equivalent of `Sessions.Events`: it scopes both `List` and `StreamRaw` to a single thread id.

```pascal
var Events := Client.Sessions.Threads.Events.List('sess_01ABCxyz', 'thd_01ZZZ',
  procedure (Params: TSessionSimpleListParams)
  begin
    Params.Limit(100);
  end);
try
  for var Evt in Events.Data do
    Display(TutorialHub, Evt);
finally
  Events.Free;
end;

var Stream := Client.Sessions.Threads.Events.StreamRaw('sess_01ABCxyz', 'thd_01ZZZ');
try
  Display(TutorialHub, Stream.Data);
finally
  Stream.Free;
end;
```

>[!NOTE]
> `Client.Sessions.Threads.StreamRaw` and `Client.Sessions.Threads.Events.StreamRaw` are different endpoints. The first emits the full thread stream (thread state + events); the second emits only the event sub-stream for that thread.

<br>

## Constraints & notes

- A session ties together one Agent + one Environment; both must exist before the session is opened. Pin the agent version (`Session.CreateAgent(Id, Version)`) for production runs that should not drift when the agent is updated.
- Three resource kinds are supported at **creation** (`Resources([...])`): GitHub repository, file, memory store. Only **file** resources can be added later via `Resources.Add` — for GitHub or memory-store resources after the fact, open a new session.
- `Resources.Update` only rotates the GitHub authorization token; all other resource fields are immutable.
- `Events.Send` accepts an **array** of events — pack interrupt + outcome + user message in a single call when you need them to land atomically.
- `StreamRaw` returns the SSE payload as raw text (`TSessionStream.Data`). It is **not** a chunked Delphi stream — wire it to your own SSE parser when you need incremental processing.
- For asynchronous completion notifications instead of polling, configure a Webhook delivery (see the [Webhooks guide](webhooks.md)).
- The `managed-agents-2026-04-01` beta header is set automatically by the wrapper.

<br>

## References

- Source unit: [`Anthropic.Sessions.pas`](../source/Anthropic.Sessions.pas)
- Helper unit: [`Anthropic.Helpers.pas`](../source/Anthropic.Helpers.pas) (`Generation.Session` — `CreateParams`, `UpdateParams`, `ListParams`, `EventListParams`, `SimpleListParams`, `SendEventsParams`, `ResourceUpdateParams`, `CreateAgent`, `CreateBranchCheckout`, `CreateCommitCheckout`, `CreateGitHubRepositoryResource`, `CreateFileResource`, `CreateMemoryStoreResource`, `CreateTextBlock`, `CreateImageBlock`, `CreateDocumentBlock`, `CreateBase64ImageSource`, `CreateUrlSource`, `CreateFileSource`, `CreateBase64DocumentSource`, `CreateTextDocumentSource`, `CreateUserMessageEvent`, `CreateInterruptEvent`, `CreateToolConfirmationEvent`, `CreateCustomToolResultEvent`, `CreateTextRubric`, `CreateFileRubric`, `CreateDefineOutcomeEvent`; array builders: `SessionResourceParts`, `SessionContentParts`, `SessionEventParts`)
- Companion guides: [Managed Agents overview](managed-agents.md), [Agents](managed-agents-agents.md), [Environments](managed-agents-environments.md), [Memory Stores](managed-agents-memory-stores.md), [Vaults](managed-agents-vaults.md), [Webhooks](webhooks.md)
