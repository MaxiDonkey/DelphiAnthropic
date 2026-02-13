# Text editor

The Text Editor tool lets Claude view and modify text files using structured edit operations.
It is suited for tasks like fixing code, refactoring, generating documentation, or creating and editing files with traceable changes.
All edits are explicit and deterministic, making file-level interactions auditable and reproducible.


- [Minimal example](#minimal-example)
- [Implementation: detailed behavior](#implementation-detailed-behavior)

___

## Minimal example

The following example shows how a model fixes a simple syntax error in a file called `primes.py`.

### Goal

Fix a for loop that is missing a :.

<br>

### Initial request

A request is sent to the model with access to the text editor tool.

```pascal
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 512;
  var Prompt := 'Fix the syntax error in primes.py.';

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Tools( ToolParts
              .Add( Tool.CreateToolTextEditor20250728)
          )
          .Messages( MessageParts
              .User(Prompt)
          );
    end; 
```

<br>

Model response (excerpt)

The model does not read the file directly.
Instead, it explicitly requests access to it:

```json
    ...
    "content": [
        {
            "type": "text",
            "text": "\n\nLet me first look at the file to identify the syntax error."
        },
        {
            "type": "tool_use",
            "id": "toolu_01Uez5dCjUx9ZLx4GYrypMrT",
            "name": "str_replace_based_edit_tool",
            "input": {
                "command": "view",
                "path": "\/repo\/primes.py"
            }
        }
    ],
    ... 
```

<br>

## Implementation: detailed behavior

<br>
