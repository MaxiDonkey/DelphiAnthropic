# Computer Use [beta]

The computer use API extends a tool-using agent by giving it an actionable ***see → decide → act → verify*** loop over a virtual desktop, which naturally complements the persistence *(memory)* and context-management mechanisms discussed earlier.

- [High-level overview of the computer use API](#high-level-overview-of-the-computer-use-api)
- [Basic Setup](#basic-setup)
- [How to implement computer use](#how-to-implement-computer-use)
- [Key takeaways](#key-takeaways)
- [Augmenting computer use with other tools](#augmenting-computer-use-with-other-tools)

___

>[!NOTE]
> This tutorial presents the minimal two-call workflow for clarity. More advanced patterns (multiple or chained tool calls) can be built on top of this foundation.

<br>

## High-level overview of the computer use API

Computer use is a beta tool that lets Claude interact with a sandboxed desktop environment through:
- **screenshots** (perception)
- **mouse actions** (click/drag/scroll, etc.)
- **keyboard actions** (typing, shortcuts)

The model does not directly control the machine. It emits tool calls, your application executes them inside an isolated environment, then returns results (screenshots, logs, errors) to the model within an **agent loop**.

<br>

## Basic Setup

A minimal integration looks like this:
1. **Pick the correct beta header** based on the model/tool version (e.g., `computer-use-2025-11-24` for Opus 4.6/4.5; otherwise `computer-use-2025-01-24`).
2. **Declare the computer tool** in the request with display geometry (`display_width_px`, `display_height_px`, `display_number`).
3. Send a request with an explicit desktop task (“Download an image,” “Open a file and edit…”)
4. Implement the loop:
   - model returns `tool_use` → client executes → client returns `tool_result` → repeat
5. Add max_iterations (cost / infinite-loop safeguard).

<br>

## How to implement computer use

Implementation means translating model intent into concrete actions and returning observable state.

### **Execution environment**

- VM/container isolation (risk reduction)
- virtual display (e.g., Xvfb) + lightweight desktop environment
- installed apps (browser, editors, etc.)

<br>

### **Action handlers**

A ***dispatcher*** mapping `action` → local execution:
- screenshot capture (base64 png/jpg)
- click / drag / scroll (coordinates)
- type / key (shortcuts)

>[!IMPORTANT]
> The current version of the `DelphiAnthropic` SDK does not provide a dispatching mechanism; this responsibility rests with the developer.

<br>

### **A robust agent loop**

- log actions + results
- propagate errors (`is_error: true`)
- add delays (slow UI)
- enforce iteration limits
- always return observable state (at least a screenshot) after sensitive steps

<br>

### **Coordinate scaling**

If screenshots are resized for vision constraints, you must map coordinates from the “analyzed image space” back to the “real screen space.”

<br>

### Usage code

```pascal
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'Save a picture of a cat to my desktop.';

  StartRun(Prompt);

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Beta(['computer-use-2025-11-24'])
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Tools( ToolParts
              .Add( Tool.Beta.CreateToolComputerUse20251124
                  .DisplayWidthPx(1024)
                  .DisplayHeightPx(768)
                  .DisplayNumber(1)
              )
              .Add( Tool.CreateToolTextEditor20250728 )
              .Add( Tool.CreateToolBash20250124 )
          )
          .Messages( MessageParts
              .User( Prompt)
          );

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous example
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

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

<br>

## Key takeaways

- **Security / prompt injection**: risk spikes with web + UI; prefer sandboxing, domain allowlists, and zero secrets.
- **Human confirmation**: actions with real-world impact (transactions, accepting terms, logins) require human approval.
- **UI reliability**: dropdowns/scrollbars can be flaky; **keyboard shortcuts** are often more reliable.
- **Explicit verification**: the model may assume success—use "action → screenshot → validation."
- **Cost / runaway control**: enforce limits (iterations/timeouts) and keep logs for debugging.

<br>

## Augmenting computer use with other tools

Most value comes from separating UI vs system vs editing:
- **bash tool**: automate non-UI operations (downloads, conversions, grep, tests, extraction) for speed and reliability.
- **text editor tool**: deterministic file edits (patch/replace), avoiding fragile manual UI editing.
- **custom tools**: encapsulate domain operations (e.g., “upload artifact,” “query DB,” “fetch weather”) to reduce web/UI exposure.

A strong pattern:
1. computer use to navigate / locate / trigger
2. bash to execute / prepare
3. text editor to apply clean edits
4. computer use to visually verify the outcome

<br>

See [official documentation](https://platform.claude.com/docs/en/agents-and-tools/tool-use/computer-use-tool)