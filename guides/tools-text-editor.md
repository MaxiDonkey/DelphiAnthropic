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

- [1. Initial request](#1-initial-request)
- [2. Model response (excerpt)](#2-model-response-excerpt)
- [3. Result provided by the application](#3-result-provided-by-the-application)
- [4. Edit result](#4-edit-result)

<br>

### 1. Initial request

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

### 2. Model response (excerpt)

The model does not read the file directly. <br>
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

### 3. Result provided by the application

The application reads the file locally and returns its contents:

- [`Primes.py`](https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/media/Primes.py): Content with errors to provide to the template
  
- Designing the payload, including integration of the Python code.

  ```pascal
    var Primes :=  System.IOUtils.TFile.ReadAllText('..\media\Primes.py');   

    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 1024;
    var Prompt := Primes;
    var FirstPrompt := 'Fix the syntax error in primes.py.';

    //JSON payload generation
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
              .User( FirstPrompt )
              .Assistant( ContentParts
                  .AddText( '\n\nLet me first look at the file to identify the syntax error.' )
                  .Add( Context.CreateToolUse
                      .Id( 'toolu_01Uez5dCjUx9ZLx4GYrypMrT' )
                      .Name( 'str_replace_based_edit_tool'  )
                      .Input( '{"command": "view", "path": "/repo/primes.py"}' )
                  )
              )
              .User( ContentParts
                  .Add( Context.CreateToolResult
                      .ToolUseId( 'toolu_01Uez5dCjUx9ZLx4GYrypMrT' )
                      .Content( Prompt )
                  )
              )
            );
    end;  
  ```

- The Delphi code responsible for creating the payload produces the following JSON.
  ```json
  {
      "model": "claude-opus-4-6",
      "max_tokens": 1024,
      "tools": [
          {
              "type": "text_editor_20250728",
              "name": "str_replace_based_edit_tool"
          }
      ],
      "messages": [
          {
              "role": "user",
              "content": "Fix the syntax error in primes.py."
          },
          {
              "role": "assistant",
              "content": [
                  {
                      "type": "text",
                      "text": "\n\nLet me first view the file to identify the syntax error."
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
              ]
          },
          {
              "role": "user",
              "content": [
                  {
                      "type": "tool_result",
                      "tool_use_id": "toolu_01Uez5dCjUx9ZLx4GYrypMrT",
                      "content": "from __future__ import annotations\r\n\r\ndef is_prime(n: int) -> bool:\r\n    \"\"\"Return True if n is prime.\"\"\"\r\n    if n <= 1:\r\n        return False\r\n    if n <= 3:\r\n        return True\r\n\r\n    # Logic issue (intentional): should be \"and\", not \"or\"\r\n    if n % 2 == 0 or n % 3 == 0:\r\n        return False\r\n\r\n    i = 5\r\n    while i * i <= n:\r\n        if n % i == 0 or n % (i + 2) == 0:\r\n            return False\r\n        i += 6\r\n    return True\r\n\r\n\r\ndef get_primes(limit: int) -> list[int]:\r\n    \"\"\"Generate primes up to limit (inclusive).\"\"\"\r\n    primes: list[int] = []\r\n\r\n    # SYNTAX ERROR (intentional): missing \":\" at end of for line\r\n    for num in range(2, limit + 1)\r\n        if is_prime(num):\r\n            primes.append(num)\r\n\r\n    return primes\r\n\r\n\r\ndef main() -> None:\r\n    limit = 50\r\n\r\n    # Logic issue (intentional): off-by-one demonstration (not critical)\r\n    prime_list = get_primes(limit - 1)\r\n\r\n    print(f\"Prime numbers up to {limit}:\")\r\n    print(prime_list)\r\n    print(f\"Found {len(prime_list)} prime numbers.\")\r\n\r\n\r\nif __name__ == \"__main__\":\r\n    main()"
                  }
              ]
          }
      ]
  }
  ```

- The model outputs the following result.
  ```bash
  I can see the syntax error on line 28: the `for` statement is missing a colon (`:`) at the end.

  Type: tool_use
  Name: str_replace_based_edit_tool
  input: {
     "command":"str_replace", "path":"/repo/primes.py", 
     "old_str":"    for num in range(2, limit + 1)", 
     "new_str":"    for num in range(2,  limit + 1):"
  }
  ```

<br>

### 4. Edit result

After applying the change, the application confirms the operation:

```json
{
  "type": "tool_result",
  "tool_use_id": "toolu_replace",
  "content": "Successfully replaced text at exactly one location."
}
```

At this point, the model can conclude and explain the fix.

## Implementation: detailed behavior

**The text editor tool relies on an explicit control loop implemented on the application side.**

### 1. The model never accesses the filesystem 

The model:
- does not read files directly
- does not modify files directly

It only ***describes intentions*** using structured commands.

<br>

### 2. Commands are explicit and typed

Each action requested by the model is one of the following commands:
- `view`: read a file or list a directory
- `str_replace`: replace an exact string
- `insert`: insert text at a specific line
- `create`: create a new file

<br>

### 3. The application executes all actions

For each tool_use request:
- a. the application extracts the command
- b. it executes the action locally
- c. it returns the result using a tool_result
- d. the tool_use_id must match exactly

<br>

### 4. Edits are intentionally local and precise

In particular for `str_replace`:
- the target string must match ***exactly***
- the edit should ideally produce ***a single match***
- ambiguous cases must be rejected or reported

This design limits side effects and makes edits auditable.

<br>

### 5. The model drives intent, the application retains control

The model:
- decides *what* to read or modify
- reasons about the content
- explains the changes

The application:
- validates paths
- applies changes
- handles security, backups, and errors

<br>

### 6. The flow is strictly sequential

Each interaction follows this sequence:
- a. model request
- b. local execution
- c. result returned
- d. next model decision

There is no hidden state and no implicit access.
