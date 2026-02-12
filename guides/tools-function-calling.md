# Function calling

Functions are user-defined tools that expose application logic to Claude through an explicit interface.
They let Claude trigger deterministic operations (e.g., computation, data access, workflows) and incorporate the returned result into its response.
Function execution is handled outside the model, preserving a clear separation between reasoning and application-side execution.

>[!NOTE]
> The code examples presented below are directly included in the `Sample.dpr` demonstration project.

- [Introduction](#introduction)
- [JSON Payload creation](#json-payload-creation)
- [Function calling](#function-calling)
- [Function calling using plugin](#function-calling-using-plugin)
- [Function calling using orchestration](#function-calling-using-orchestration)
- [Key Takeaways](#key-takeaways)
- [Implementation Checklist](#implementation-checklist)

___

>[!NOTE]
> This tutorial presents the minimal two-call workflow for clarity.
> More advanced patterns (multiple or chained tool calls) can be built on top of this foundation.

## Introduction

>The code snippets will exclusively refer to the `procedure (Params: TChatParams)`, as introduced in the sections covering non-streamed and streamed generation.

### Function calling follows a two-step workflow:

- Step 1 - definition and tool selection: you build a JSON request that includes the user question and one or more function definitions. The model decides whether a function call is required and, if so, returns the function name to invoke along with the corresponding arguments. When multiple functions are provided, the model selects the one it considers the most appropriate.

- Step 2: - execution and final response: you submit a second request that injects the selected function name and the arguments returned in the previous step. This second pass produces a final response that incorporates the values obtained from Step 1.

>[!IMPORTANT]
>The `DelphiAnthropic` wrapper supports two ways to handle function calling:
>- a plugin-based approach, which is well-suited for large catalogs of functions with more complex processing;
>- a direct approach, where the function is implemented explicitly in application code.

The example used throughout this section is: "What's the weather like in San Francisco?" For Step 1, three variants are presented to illustrate the flexibility of `DelphiAnthropic` in this workflow.

<br>

## JSON Payload creation

Before executing a function call, it is necessary to build a JSON payload that describes both the user request and the functions exposed to the model.

### Expected JSON payload

```json
{
    "model": "claude-opus-4-6",
    "max_tokens": 1024,
    "tools": [
        {
            "type": "custom",
            "name": "get_weather",
            "description": "Get the current weather in a given location",
            "input_schema": {
                "type": "object",
                "properties": {
                    "location": {
                        "type": "string",
                        "description": "The city and state, e.g. San Francisco, CA"
                    },
                    "unit": {
                        "type": "string",
                        "enum": [
                            "celsius",
                            "fahrenheit"
                        ]
                    }
                },
                "required": [
                    "location"
                ]
            }
        }
    ],
    "messages": [
        {
            "role": "user",
            "content": "What's the weather like in San Francisco?"
        }
    ]
}
```

The JSON payload above represents the canonical form; the following sections demonstrate three equivalent ways to map this structure into Delphi code.

- [Approach using the `TSchemaParams` class and the `Generation` record helper](#approach-using-the-tschemaparams-class-and-the-generation-record-helper)
- [Approach using only the `Generation` record helper](#approach-using-only-the-generation-record-helper)
- [Approach based on using a string](#approach-based-on-using-a-string)

<br>

### Approach using the `TSchemaParams` class and the `Generation` record helper

```pascal
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'What''s the weather like in San Francisco?';

  // Schema Payload creation using TSchemaParams class
  var GetWeather := TSchemaParams.New
    .&Type('object')
    .Properties( TJSONObject.Create
       .AddPair('location', TJSONObject.Create
           .AddPair('type', 'string')
           .AddPair('description', 'The city and state, e.g. San Francisco, CA')
       )
       .AddPair('unit', TJSONObject.Create
           .AddPair('type', 'string')
           .AddPair('enum', TJSONArray.Create
               .Add('celsius')
               .Add('fahrenheit'))
       )
    )
    .Required(['location']);

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Tools( ToolParts
              .Add( Tool.CreateToolCustom
                  .Name('get_weather')
                  .Description('Get the current weather in a given location')
                  .InputSchema(GetWeather)
              )
          )
          .Messages( MessageParts
              .User( Prompt )
          );
    end;
```

<br>

### Approach using only the `Generation` record helper

```pascal
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'What''s the weather like in San Francisco?';

  // Schema Payload creation 
  var JSONGetWeather :=
    '''
    {
        "type": "object",
        "properties": {
            "location": {
                "type": "string",
                "description": "The city and state, e.g. San Francisco, CA"
            },
            "unit": {
                "type": "string",
                "enum": [
                    "celsius",
                    "fahrenheit"
                ]
            }
        },
        "required": [
            "location"
        ]
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
          .Tools( ToolParts
              .Add( Tool.CreateToolCustom
                  .Name('get_weather')
                  .Description('Get the current weather in a given location')
                  .InputSchema(JSONGetWeather)
              )
          )
          .Messages( MessageParts
              .User( Prompt )
          );
    end;
```

With this approach, declaring multiple functions is both efficient and visually clear. Additional functions can be added easily by calling `.Add(...)`.

>[!WARNING]
> his multiline string–based approach is only applicable when using Delphi version 12 or later. Otherwise, you can still rely on an external string in which the JSON payload is explicitly defined.

<br>

### Approach based on using a string

```pascal
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'What''s the weather like in San Francisco?';

  // Schema Payload creation 
  var JSONTools :=
    '''
    [
        {
            "type": "custom",
            "name": "get_weather",
            "description": "Get the current weather in a given location",
            "input_schema": {
                "type": "object",
                "properties": {
                    "location": {
                        "type": "string",
                        "description": "The city and state, e.g. San Francisco, CA"
                    },
                    "unit": {
                        "type": "string",
                        "enum": [
                            "celsius",
                            "fahrenheit"
                        ]
                    }
                },
                "required": [
                    "location"
                ]
            }
        }
    ]
    ''';

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Tools(JSONTools)
          .Messages( MessageParts
              .User( Prompt )
          );
    end;
```

>[!WARNING]
> This multiline string–based approach is only applicable when using Delphi version 12 or later. Otherwise, you can still rely on an external string in which the JSON payload is explicitly defined.

<br>

## Function calling

We will rely on the previously discussed point regarding the creation of the JSON payload. For details on how to construct this `Payload`, please refer to the [JSON Payload Creation](#json-payload-creation) section.

### Run function calling

```pascal
  // Asynchronous example
  // Execute the first step
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value); // <-- Execute the second step with the display method
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

With this asynchronous approach, the `Display` method handles the second step by:
- processing the arguments returned by the input property (this processing is performed algorithmically on the client side);
- submitting the transformed information to the model for efficient utilization. 


<br>

### Code: Detailed Approach

- **Method Display**

  ```pascal
  procedure Display(Sender: TObject; Value: TChat);
  begin
    if not Assigned(Value) then
      Exit;

    for var Item in Value.Content do
      begin
       ... 
       if Item.&Type = TContentBlockType.tool_use then  
         begin
           TutorialHub.ToolCall(Item.Input); // <-- Execute the second step
         end;
      end;
  end;
  ```

- `ToolCall` call these methods:

  ```pascal
  // Process the arguments provided during the first step 
  function TFMXTutorialHub.WeatherRetrieve(const Value: string): string;
  begin
    var Location := TJsonReader.Parse(Value).AsString('location');

    if Location.ToLower.Contains('san francisco') then
      begin
        var Json := TJSONObject.Create;
        try
        Result := Json
          .AddPair('Location', 'San Francisco, CA')
          .AddPair('temperature', '16°C')
          .AddPair('forecast', 'rainy, low visibility but sunny in the late afternoon or early evening')
          .ToJSON;
        finally
         Json.Free;
        end;
      end;
    ...
  end;

  // Execute the second step and display the resulting output.
  procedure TFMXTutorialHub.WeatherReporter(const Value: string);
  begin
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 1024;
    var Prompt := 'Announce the day''s weather forecast';

    if Tool = nil then
      Prompt := Prompt + sLineBreak + WeatherRetrieve(Value)
    else
      Prompt := Prompt + sLineBreak + Tool.Execute(Value);  // using plugin

    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Messages( MessageParts
                .User( Prompt)
            );
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
  end;
  ```

>[!NOTE]
> The Display methods are available in the support units Anthropic.Tutorial.FMX.pas and Anthropic.Tutorial.VCL.pas. These units are intended to facilitate the reader’s skill development.

<br>

## Function calling using plugin

In this section, we will use an approach in which the function call is encapsulated within a ***plugin***. Refer to the following units:
- **Anthropic.Functions.Core.pas:** definition of the `IFunctionCore` interface required to create the plugin;
- **Anthropic.Functions.Example.pas:** implementation of the `IFunctionCore` interface.

The second step is once again handled by the `Display` method, as described in the previous section.

```pascal
  // Create a plugin instance
  var FunctionPlugin := TWeatherReportFunction.CreateInstance;
  TutorialHub.Tool := FunctionPlugin;

  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'What''s the weather like in Paris?';

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Tools( ToolParts
              .Add( Tool.CreateToolCustom
                  .FunctionPlugin(FunctionPlugin)
              )
          )
          .Messages( MessageParts
              .User( Prompt )
          );
    end;

  // Asynchronous example
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value); // <-- Execute the second step with the display method
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

## Function calling using orchestration

We will rely on the previously discussed point regarding the creation of the JSON payload. For details on how to construct this payload, please refer to the [JSON Payload Creation](#json-payload-creation) section.

Now define the method that ***orchestrates the two promises*** required to retrieve the weather for San Francisco.

>[!NOTE]
>Using `TPromise<TChat>` requires adding the `Anthropic.Async.Promise` unit to the uses clause.

```pascal
  TutorialHub.Tool := nil;

  // Asynchronous example
  // Execute the first step
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  Promise
    .&Then(
      function (Value: TChat): TPromise<TChat>
      var
        Arguments: string;
      begin
        Result := nil;
        for var Item in Value.Content do
        begin
          if Item.&Type = TContentBlockType.tool_use then
            begin
              Arguments := Item.Input;

              // Execute the second step
              Result := Client.Chat.AsyncAwaitCreate(
                procedure (Params: TChatParams)
                begin
                  Params
                    .Model(ModelName)
                    .MaxTokens(MaxTokens)
                    .Messages( Generation.MessageParts
                        .User('Announce the day''s weather forecast : ' + TutorialHub.WeatherRetrieve(Arguments))
                    )
                end);

            end;
        end;
      end)
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

<br>

## Key Takeaways

- Tool use formalizes the boundary between model reasoning and deterministic execution.
- Claude does not execute functions directly; it emits structured intents (`tool_use`).
- The interaction cycle is always ***decision → execution → integration***.
- Tool descriptions matter more than complex schemas.
- JSON schemas guide both the model and the orchestration layer.
- Parallel tool calls are supported and encouraged for independent operations.
- Orchestration automates chaining, error handling, and context management.
- `tool_result` blocks ensure traceability and conversational continuity.

<br>

## Implementation Checklist

- Identify the external capabilities to expose (computation, I/O, workflows).
- Define each tool with:
  - a stable name,
  - a precise description (when to use / when not to use),
  - a clear and minimal JSON `input_schema`.
- Inject the tools into the initial request (tools).
- Let the model decide whether a tool invocation is required.
- Detect `tool_use` blocks in the model response.
- Execute the corresponding function on the application or server side.
- Return the results using a `tool_result` block.
- Integrate the results into the final model response.
- Explicitly handle errors, timeouts, and schema validation.
- (Optional) Use an orchestrator for complex or parallel workflows.