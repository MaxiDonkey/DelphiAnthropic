# Structured outputs

Structured outputs make responses reliable, automatically verifiable, and easy to consume downstream, which is essential for agent workflows and production integrations.

- [JSON outputs](#json-outputs)
- [Strict tool use](#strict-tool-use)
- [Using both features together](#using-both-features-together)
- [Limitations and invalid outputs](#limitations-and-invalid-outputs)
- [Quick Selection Guide](#quick-selection-guide)
- [Practical Notes](#practical-notes)

___

## JSON outputs

***JSON outputs*** constrain the model to return valid JSON that matches a provided schema.
They prevent common failures (invalid JSON, missing required fields, inconsistent types) via constrained decoding.
Result: *safe parsing*, *guaranteed types*, and *fewer validation/retry loops* in your application.

They’re well-suited for ([see](https://platform.claude.com/docs/en/build-with-claude/structured-outputs#common-use-cases)):
- information extraction 
- structured reports
- API-ready responses.

<br>

### Using TSchemaParams

- JSON Payload creation 

  ```pascal
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 1024;
    var Prompt := 'Extract the key information from this email: John Smith (john@example.com) is interested in our Enterprise plan and wants to schedule a demo for next Tuesday at 2pm.';

    // Schema Payload creation using TSchemaParams class
    var SchemaPayload := TSchemaParams.New
      .&Type('object')
      .Properties( TJSONObject.Create
         .AddPair('name', TJSONObject.Create
             .AddPair('type', 'string')
         )
         .AddPair('email', TJSONObject.Create
             .AddPair('type', 'string')
         )
         .AddPair('plan_interest', TJSONObject.Create
             .AddPair('type', 'string')
         )
         .AddPair('demo_requested', TJSONObject.Create
             .AddPair('type', 'boolean')
         )
      )
      .Required(['name', 'email', 'plan_interest', 'demo_requested'])
      .AdditionalProperties(False);

    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Messages( MessageParts
                .User( Prompt )
            )
            .OutputConfig( CreateOutputConfig
                .Format( CreateFormat
                    .Schema( SchemaPayload )
                )
            );
      end;
  ```

- JSON Payload generated

  ```json
  {
      "model": "claude-opus-4-6",
      "max_tokens": 1024,
      "messages": [
          {
              "role": "user",
              "content": "Extract the key information from this email: John Smith (john@example.com) is interested in our Enterprise plan and wants to schedule a demo for next Tuesday at 2pm."
          }
      ],
      "output_config": {
          "format": {
              "type": "json_schema",
              "schema": {
                  "type": "object",
                  "properties": {
                      "name": {
                          "type": "string"
                      },
                      "email": {
                          "type": "string"
                      },
                      "plan_interest": {
                          "type": "string"
                      },
                      "demo_requested": {
                          "type": "boolean"
                      }
                  },
                  "required": [
                      "name",
                      "email",
                      "plan_interest",
                      "demo_requested"
                  ],
                  "additionalProperties": false
              }
          }
      }
  }
  ```

<br>

### With Delphi version 12 and above - using multi-line strings

- JSON Payload creation 

  ```pascal
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 1024;
    var Prompt := 'Extract the key information from this email: John Smith (john@example.com) is interested in our Enterprise plan and wants to schedule a demo for next Tuesday at 2pm.';

    // Schema Payload creation using multi-lines string
    var SchemaPayload :=
      '''
      {
          "type": "object",
          "properties": {
            "name": {"type": "string"},
            "email": {"type": "string"},
            "plan_interest": {"type": "string"},
            "demo_requested": {"type": "boolean"}
          },
          "required": ["name", "email", "plan_interest", "demo_requested"],
          "additionalProperties": false
      }
      ''';

    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Messages( MessageParts
                .User( Prompt )
            )
            .OutputConfig( CreateOutputConfig
                .Format( CreateFormat
                    .Schema( SchemaPayload )
                )
            );
      end;
  ```

See [Official Documentation](https://platform.claude.com/docs/en/build-with-claude/structured-outputs#json-outputs)

<br>

## Strict tool use

***Strict tool*** use ensures tool calls exactly match the specified input schema.
The model can’t invent parameters or pass incorrectly typed values.
**This makes multi-step agents more robust:** each function receives valid inputs without extra guardrails.
It’s a key building block for ***reliable agentic systems*** at scale.

<br>

## Using both features together

JSON outputs and strict tool use are complementary:
- JSON outputs control the format of the model’s final response,
- strict tool use controls the validity of intermediate tool calls.

Together, they enable agents that can orchestrate tools with schema-safe parameters while returning a final, structured result that applications can consume directly.

<br>

## Limitations and invalid outputs

Supported JSON Schema features are intentionally limited (e.g., no recursion, no fine-grained numeric constraints, limited regex support).
Even so, some cases can produce outputs that don’t match your schema:
- safety refusals (the refusal message overrides schema constraints),
- token limit truncation,
- overly complex schemas triggering 400-level validation errors.

Finally, structured outputs can add first-use latency due to grammar compilation and are incompatible with some features like citations.

<br>

## Quick Selection Guide

<br>

## Practical Notes