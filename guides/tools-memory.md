# Memory Tool

The memory API provides Claude with an explicit, persistent, and user-controlled memory, decoupling agent continuity from the context window while remaining entirely client-managed.

- [High-level overview of the memory API](#high-level-overview-of-the-memory-api)
- [How to implement the memory API](#how-to-implement-the-memory-api)
  - [Usage example](#usage-example)
- [Memory tool commands](#memory-tool-commands)
- [Using context editing with the memory tool](#using-context-editing-with-the-memory-tool)
- [Why use compaction with the memory tool](#why-use-compaction-with-the-memory-tool)
- [Implementation — detailed behavior](#implementation--detailed-behavior)

___

<br>

## High-level overview of the memory API

The memory API is an ***external persistence mechanism*** that allows Claude to read from and write to a `/memories` directory managed by the client application.
Unlike opaque internal memory, it is:
- **Deterministic:** only explicitly written data is preserved
- **Auditable:** files are readable, editable, and deletable
- **Persistent across conversations**
- **Stateless on the model side**, stateful on the application side

It turns Claude into an ***agent with explicit memory***, without sacrificing user control.

<br>

## How to implement the memory API

Implementation relies on three core components:

### API-side activation
- Required beta header: `context-management-2025-06-27`
- Inclusion of the `memory_20250818` tool in requests
- Limited to compatible Claude 4.x models

### Client-side memory backend
The application must implement handlers for:
- reading (`view`)
- writing (`create`, `insert`, `str_replace`)
- maintenance (`rename`, `delete`)

> Claude **does not store anything itself**: it emits intentions, your infrastructure executes them.

### Usage discipline
Without explicit guidance, Claude may over-write or over-persist. <br>
Best practice is ***short, structured, evolving memory*** (state over verbatim).

### Usage example

This example shows how to integrate Claude’s memory tool to manage persistent application-side memory using local files.

The interaction follows three steps:

>[!NOTE]
> The code example relies on context reconstruction from the interaction history (see [this section](tools-text-editor.md#using-context-builder-from-history)).

1. Claude first reads the `/memories` directory to check whether user data already exists.

    ```pascal
      TutorialHub.ToolTurns := TTurns.CreateInstance;  // <--- Instanciate ToolTurns: ITurns

      var ModelName := 'claude-opus-4-6';
      var MaxTokens := 1024;
      var SystemPrompt := 'Store facts about the user and preferences in /memories as XML. Before responding, check memory. Keep it up to date.';
      var Prompt := 'Maintain a persistent profile about me (interests, current work). Initialize it if necessary.';

      //JSON payload creation
      var Payload: TChatParamProc :=
        procedure (Params: TChatParams)
        begin
          with Generation do
            Params
              .Beta(['context-management-2025-06-27'])
              .Model(ModelName)
              .MaxTokens(MaxTokens)
              .System(SystemPrompt)
              .Messages( MessageParts
                  .User( Prompt )
              )
              .Tools( ToolParts
                  .Add( Tool.Beta.CreateMemoryTool20250818 )
              );
        end;

      // Asynchronous example
      var Promise := Client.Chat.AsyncAwaitCreate(Payload);

      Promise
        .&Then(
          procedure (Value: TChat)
          begin
            var TurnItem := TutorialHub.ToolTurns.AddItem(Value);  // <--- Save first turn into ToolTurns
            Display(TutorialHub, TurnItem);
            Display(TutorialHub, Value);
          end)
        .&Catch(
          procedure (E: Exception)
          begin
            Display(TutorialHub, E.Message);
          end);
    ```

- JSON Result (exerpt)
  ```json
  {
     "type": "tool_use",
     "id": "toolu_01CUDKsXTaAJvEFQ3d2eQ4Vx",
     "name": "memory",
     "input": {
         "command": "view",
         "path": "\/memories"
     }               
  }
  ```

<br>

2. If needed, it requests the creation of a `user.xml` file to store the user profile.

    ```pascal
      var ModelName := 'claude-opus-4-6';
      var MaxTokens := 1024;
      var SystemPrompt := 'Store facts about the user and preferences in /memories as XML. Before responding, check memory. Keep it up to date.';
      var Prompt := 'Directory: /memories';

      var Turns := TutorialHub.ToolTurns;

      StartRun(Prompt);

      //JSON payload creation
      var Payload: TChatParamProc :=
        procedure (Params: TChatParams)
        begin
          with Generation do
            Params
              .Beta(['context-management-2025-06-27'])
              .Model(ModelName)
              .MaxTokens(MaxTokens)
              .System(SystemPrompt)
              .Tools( ToolParts
                  .Add( Tool.Beta.CreateMemoryTool20250818 )
              )
              .Messages( Turns
                  .BuildContextFromHistory('memory', Prompt) );  // <--- Automatic context reconstruction
        end;

      // Asynchronous creation (promise-based)
      var Promise := Client.Chat.AsyncAwaitCreate(Payload);

      // Simple processing or orchestration of promise
      Promise
        .&Then(
          procedure (Value: TChat)
          begin
            var TurnItem := TutorialHub.ToolTurns.AddItem(Value);  // <--- Save second turn into ToolTurns
            Display(TutorialHub, TurnItem);
            Display(TutorialHub, Value);
          end)
        .&Catch(
          procedure (E: Exception)
          begin
            Display(TutorialHub, E.Message);
          end);
    ```

- JSON Result (exerpt)
  ```json
  {
            "type": "tool_use",
            "id": "toolu_015EmeiiMzsDceR7cEGeUeW2",
            "name": "memory",
            "input": {
                "command": "create",
                "path": "\/memories\/user_profile.xml",
                "file_text": "<user_profile>\n  <last_updated>2025-01-24<\/last_updated>\n\n  <interests>\n    <!-- Add interests as they are shared -->\n  <\/interests>\n\n  <current_work>\n    <!-- Add current work\/projects as they are shared -->\n  <\/current_work>\n\n  <preferences>\n    <!-- Add preferences as they are shared -->\n  <\/preferences>\n\n  <background>\n    <!-- Add background info as it is shared -->\n  <\/background>\n\n  <notes>\n    <!-- Miscellaneous notes -->\n  <\/notes>\n<\/user_profile>\n"
            }
    }
  ```

<br>

3. Once the file is created, the conversation continues normally, and Claude can read and update this file in later messages.

    ```pascal
      var ModelName := 'claude-opus-4-6';
      var MaxTokens := 1024;
      var SystemPrompt := 'Store facts about the user and preferences in /memories as XML. Before responding, check memory. Keep it up to date.';
      var Prompt := 'File created successfully at /memories/user.xml';

      var Turns := TutorialHub.ToolTurns;

      //JSON payload creation
      var Payload: TChatParamProc :=
        procedure (Params: TChatParams)
        begin
          with Generation do
            Params
              .Beta(['context-management-2025-06-27'])
              .Model(ModelName)
              .MaxTokens(MaxTokens)
              .System(SystemPrompt)
              .Tools( ToolParts
                  .Add( Tool.Beta.CreateMemoryTool20250818 )
              )
              .Messages( Turns
                  .BuildContextFromHistory('memory', Prompt) );  // <--- Automatic context reconstruction
        end;

      // Asynchronous creation (promise-based)
      var Promise := Client.Chat.AsyncAwaitCreate(Payload);

      // Simple processing or orchestration of promise
      Promise
        .&Then(
          procedure (Value: TChat)
          begin
            Display(TutorialHub, Value);
          end)
        .&Catch(
          procedure (E: Exception)
          begin
            Display(TutorialHub, E.Message);
          end);
    ```

- Result 

  ```text
  Your persistent profile has been initialized! 🎉

  Right now it's a blank slate — ready to be filled in as we chat. Here's what I'll track for you:

  - **Interests** – hobbies, topics you're curious about, things you enjoy
  - **Current Work** – projects, job, tasks you're focused on
  - **Preferences** – communication style, tool preferences, etc.
  - **Background** – relevant experience, skills, education
  - **Notes** – anything else worth remembering

  Feel free to share anything about yourself now, or it'll naturally build up over our conversations. What would you like to tell me first?
  ```

<br>

## Memory tool commands

The commands form a ***minimal, intentionally constrained filesystem API***.

### Reading 
- `view`:
  - lists a directory
  - or displays a file with line numbers
  - optional pagination via `view_range`

### Writing / editing
- `create`: create a new file
- `insert`: insert text at a given line
- `str_replace`: verbatim, unique replacement (safety-oriented)

### Maintenance
- `rename`: rename or move a file/directory
- `delete`: recursive deletion

>[!IMPORTANT]
> Key point: **no implicit writes** — every operation is explicit and traceable.

<br>

## Using context editing with the memory tool

Context editing automatically **cleans the active context when it grows too large.**

**When combined with memory:**
1. Claude receives a warning that cleanup is imminent
2. It serializes critical state into `/memories`
3. Older tool results are cleared
4. Work continues by reloading persistent memory

**Outcome:**
- long-running, agentic workflows
- minimal active context
- preserved logical continuity

Memory effectively acts as an explicit checkpoint layer.

<br>

## Why use compaction with the memory tool

Compaction operates at a different level than context editing:
- **Compaction:** server-side summarization of conversational history
- **Memory:** client-side persistence of critical state

Why combine them:
- Compaction ***reduces narrative noise***
- Memory ***preserves structural information***
- Together they prevent:
  - context drift
  - loss of key decisions
  - token explosion

>[!NOTE] 
>Ideal architecture for long-running agents: <br>
> **Memory = facts / state / decisions** <br>
> **Compaction = semantic continuity**

<br>

## Implementation — detailed behavior

At runtime, the memory-enabled agent follows a deterministic sequence:

1. **Pre-task initialization**
   The agent always reads `/memories` first to recover persisted state before reasoning or planning.

2. **Task execution with memory awareness**
   During execution, the agent treats memory as authoritative state, not conversational context.
   Temporary reasoning stays in-context; durable facts, decisions, and checkpoints are externalized.
3. **Selective persistence**
   Only state transitions are written to memory (not raw dialogue or intermediate thoughts).
   Writes are explicit, minimal, and structured.
4. **Context pressure handling**
   When notified of impending context cleanup, the agent serializes critical state into memory before loss.
5. **Resumption semantics**
   After context editing or compaction, execution resumes by reloading memory as ground truth, ensuring continuity.

Invariant: memory is explicit, append-light, and authoritative; context is ephemeral and lossy by design.
