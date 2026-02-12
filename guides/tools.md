# Tools

Tools are the mechanism that allows Claude to go beyond text generation and take action, by orchestrating external functions in a controlled, typed, and conversation-integrated way.

- [Using tools with Claude](#using-tools-with-claude)
- [How tool use works](#how-tool-use-works)
- [How to implement tool use](#how-to-implement-tool-use)
- [Stable tool categories](#stable-tool-categories)

___

## Using tools with Claude

Using tools means ***explicitly exposing a set of operational capabilities*** to Claude (client tools, server tools, or MCP tools), each defined by a name, semantics, and an input schema.
Claude does not execute these functions directly: ***it reasons about when to use them***, constructs structured calls (`tool_use`), and incorporates their results (`tool_result`) into its final response.

This approach enables:
- extending the model’s effective capabilities (data access, computation, actions),
- a clear separation between model reasoning and software execution,
- reliable structured exchanges via JSON Schema (with optional strict validation).

<br>

## How tool use works

Tool use follows a ***decision → execution → integration*** cycle, natively embedded in the conversational format.
1. **Decision**
   Based on the user prompt and available tools, Claude decides whether it can answer directly or needs to invoke one or more tools (sequentially or in parallel).
2. **Invocation**
   Claude emits one or more `tool_use` blocks, each representing a tool call with parameters conforming to the declared schema.
3. **External execution**
   - For client tools, the host application executes the function and returns the result.
   - For server tools, Claude executes them directly on Anthropic’s infrastructure.
4. **Integration**
   Results are returned to Claude via `tool_result` blocks, which the model then uses to generate the final natural-language answer.

This mechanism guarantees:
- strict traceability of tool calls,
- compatibility with parallel execution,
- uninterrupted conversational continuity.

<br>

## How to implement tool use

Implementation consists of encapsulating business logic as declarative tools, then managing their lifecycle through the Messages API.

### Core steps

1. **Define tools**
   - Stable name
   - Detailed description (when to use / when not to use)
   - JSON Schema `input_schema` (optionally with `strict: true`)

2. **Inject tools into the request**
   Tools are provided via the `tools` parameter alongside the user prompt.
  
3. **Handle tool calls**
   - Detect `tool_use` blocks
   - Execute the corresponding function
   - Return results via properly formatted `tool_result` blocks

4. **Automate orchestration (optional but recommended)**
   The tool runner abstracts the execution loop (execution, error handling, parallelism, context compaction), significantly reducing application complexity.

### Key best practices
- Rich tool descriptions matter more than complex schemas
- Always return all tool results in a single user message
- Explicitly encourage parallel calls for independent operations
- Use strict validation in production environments

### Unifying thread

The three levels — **using, understanding, and implementing tools** — describe the same idea from complementary angles:

>Tools are a formal interface between the model’s probabilistic reasoning and deterministic system actions.

<br>

## Stable tool categories

These tools represent the core primitives available for extending Claude beyond text generation.
They define the main ways in which the model can interact with external systems in a stable and supported manner.

- [Functions](#functions)
- [Bash Tool](#bash-tool)
- [Text Editor Tool](#text-editor-tool)
- [Web Search Tool](#web-search-tool)

___

### Functions

Functions are user-defined tools that expose application logic to Claude through an explicit interface.
They let Claude trigger deterministic operations (e.g., computation, data access, workflows) and incorporate the returned result into its response.
Function execution is handled outside the model, preserving a clear separation between reasoning and application-side execution.

To facilitate the adoption of this feature, a dedicated tutorial has been designed for this purpose. Please refer to the [Function Calling Tutorial](tools-function-calling.md#function-calling).
<br>

### Bash Tool

The Bash tool provides access to a persistent shell session for command-line automation.
It enables system operations such as running scripts, inspecting environments, and working with files via standard shell commands.
Session state is preserved across commands, allowing multi-step workflows to be expressed naturally.


#### Code example

```pascal
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'Using PowerShell, display the list of executable (.exe) files in the current directory';

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Tools( ToolParts
              .Add( Tool.CreateToolBash20250124 )
          )
          .Messages( MessageParts
              .User( Prompt )
          );
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

  // Synchronous example
//  var Value := Client.Chat.Create(Payload);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

- JSON result

  ```json
  {
    "model": "claude-opus-4-6",
    "id": "msg_018JN634XZPNFgKQHweRgsa1",
    "type": "message",
    "role": "assistant",
    "content": [
        {
            "type": "text",
            "text": "\n\nHere's how to display the list of executable (`.exe`) files in the current directory using PowerShell:"
        },
        {
            "type": "tool_use",
            "id": "toolu_01RDAjuY1QPHSjHpz6dxoGQn",
            "name": "bash",
            "input": {
                "command": "pwsh -Command \"Get-ChildItem -Path . -Filter *.exe\""
            }
        }
    ],
    "stop_reason": "tool_use",
    "stop_sequence": null,
    "usage": {
        "input_tokens": 766,
        "cache_creation_input_tokens": 0,
        "cache_read_input_tokens": 0,
        "cache_creation": {
            "ephemeral_5m_input_tokens": 0,
            "ephemeral_1h_input_tokens": 0
        },
        "output_tokens": 94,
        "service_tier": "standard",
        "inference_geo": "global"
    }
  }
  ```

The `input` property contains the value `{"command": "pwsh -Command \"Get-ChildItem -Path . -Filter *.exe\""}`.

This value corresponds to a PowerShell command, `Get-ChildItem -Path . -Filter *.exe`, whose purpose is to list the executable files in the current directory.

<br>

#### Essential Points to Understand About the Bash Tool

- **Execution of real bash commands**
  The tool can run actual shell commands (`ls`, `pip`, scripts, etc.).

- **Persistent session**
  State is preserved across commands: working directory, created files, environment variables.
- **Schema-less tool interface**
  Inputs are free-form (`{ "command": "…" }`) with no built-in structural validation.
- **High attack surface**
  ***Grants near-direct system access if not tightly constrained.***
- **Security is the implementer’s responsibility**
  Requires isolation (Docker/VM), command filtering, timeouts, resource limits, and logging.
- **Blocking dangerous commands is mandatory**
  Examples: `rm -rf`, `sudo`, fork bombs, disk formatting.
- **Explicit error handling**
  Documented cases include timeouts, command not found, and permission errors.
- **Functional limitations**
  No interactive commands (`vim`, `less`), no GUI applications, no streaming output, large outputs must be truncated.
- **Session scope is limited**
  Persistence exists only within the current session, not across separate API calls.
- **Non-trivial token cost**
  Tool activation and command outputs (stdout/stderr) consume tokens.
- **Intended use cases**
  Automation, CI/CD pipelines, data processing, and reproducible scripting.
- **Not suitable for free-form shell exploration**
  Designed for ***controlled agent workflows***, not as a human-like terminal.
- **Implicit takeaway**
  Using this tool is equivalent to exposing a remote shell and therefore ***requires strict governance***.

<br>

### Text Editor Tool

The Text Editor tool lets Claude view and modify text files using structured edit operations.
It is suited for tasks like fixing code, refactoring, generating documentation, or creating and editing files with traceable changes.
All edits are explicit and deterministic, making file-level interactions auditable and reproducible.

<br>

### Web Search Tool

The Web Search tool gives Claude access to current web content beyond its training cutoff.
It supports up-to-date, source-backed answers by retrieving information online and returning responses with citations.
Search execution and source attribution are handled natively, ensuring traceability of external information.
