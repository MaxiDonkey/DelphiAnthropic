# Tool search [beta]

The Tool Search API separates tool discovery from tool execution to make LLM-based systems scalable as the number of tools grows.

- [The problem this addresses](#the-problem-this-addresses)
- [The architectural principle](#the-architectural-principle)
- [What actually happens](#what-actually-happens)
- [Key levers to be aware of](#key-levers-to-be-aware-of)
- [When this approach makes sense (and when it doesn’t)](#when-this-approach-makes-sense-and-when-it-doesnt)
- [Key takeaway](#key-takeaway)
- [Usage code](#usage-code)

___

<br>

## The problem this addresses

Traditional tool calling works well as long as the number of tools remains limited.
As the tool catalog grows, several issues emerge:
- tool definitions consume a significant portion of the context
- selecting the relevant tool becomes less reliable
- adding a new tool can degrade overall system behavior

These limitations are not caused by poor implementation, but by the fact that the model must reason over ***all tools at once***.

<br>

## The architectural principle

The core idea is to **decouple tool discovery from tool execution**.

Instead of exposing all tools to the model:
- the model is given access to **a tool search mechanism**
- actual tools are loaded **only when they are relevant**

In other words, the model no longer selects a tool from a complete catalog. <br>
It first **narrows the tool space**, then acts.

<br>

## What actually happens

In a system using tool search, the flow is:
1. The model determines that a tool is required
2. It formulates a tool search query
3. The system returns a small set of candidate tools
4. The definitions of those tools are injected dynamically
5. The model calls the selected tool

From the model’s perspective, the final step is a standard tool call. <br>
The difference happens **upstream**, during tool selection.

<br>

## Key levers to be aware of

Without going into implementation details, three concepts are enough to understand the impact:
- **Deferred loading**: most tools are not visible by default
- **Controlled vs. flexible search**: tool selection can be rule-based (e.g. regex) or language-based
- **Always-available tools**: a small number of critical tools can remain permanently visible

These levers allow control over:
- context size
- selection accuracy
- system stability as the tool catalog grows

<br>

## When this approach makes sense (and when it doesn’t)

This architecture is well suited when:
- the number of tools is large or expected to grow
- tools are not all required for every request
- tool selection itself becomes a problem

It is usually unnecessary when:
- the system has only a few tools
- all tools are always needed
- simplicity matters more than scalability

<br>

## Key takeaway

Tool search is not a minor optimization. <br>
It represents a **shift in mental model**:
> instead of asking the LLM to know everything, we teach it how to find what it needs.

<br>

## Usage code

- Expected JSON payload
  ```json
  {
        "model": "claude-opus-4-6",
        "max_tokens": 2048,
        "messages": [
            {
                "role": "user",
                "content": "What is the weather in San Francisco?"
            }
        ],
        "tools": [
            {
                "type": "tool_search_tool_regex_20251119",
                "name": "tool_search_tool_regex"
            },
            {
                "name": "get_weather",
                "description": "Get the weather at a specific location",
                "input_schema": {
                    "type": "object",
                    "properties": {
                        "location": {"type": "string"},
                        "unit": {
                            "type": "string",
                            "enum": ["celsius", "fahrenheit"]
                        }
                    },
                    "required": ["location"]
                },
                "defer_loading": true
            },
            {
                "name": "search_files",
                "description": "Search through files in the workspace",
                "input_schema": {
                    "type": "object",
                    "properties": {
                        "query": {"type": "string"},
                        "file_types": {
                            "type": "array",
                            "items": {"type": "string"}
                        }
                    },
                    "required": ["query"]
                },
                "defer_loading": true
            }
        ]
    }
  ```

- Delphi approach

  ```pascal
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 2048;
    var Prompt := 'What is the weather in San Francisco?';

    // Schema Payload creation using TSchemaParams class
    var GetWeather := TSchemaParams.New
      .&Type('object')
      .Properties( TJSONObject.Create
         .AddPair('location', TJSONObject.Create
             .AddPair('type', 'string')
         )
         .AddPair('unit', TJSONObject.Create
             .AddPair('type', 'string')
             .AddPair('enum', TJSONArray.Create
                 .Add('celsius')
                 .Add('fahrenheit'))
         )
      )
      .Required(['location']);

    var GetSearchFiles := TSchemaParams.New
      .&Type('object')
      .Properties( TJSONObject.Create
         .AddPair('query', TJSONObject.Create
             .AddPair('type', 'string')
         )
         .AddPair('file_types', TJSONObject.Create
             .AddPair('type', 'array')
             .AddPair('items', TJSONObject.Create
                 .AddPair('type', 'string')
             )
         )
      )
      .Required(['query']);

    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Beta(['advanced-tool-use-2025-11-20'])
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Messages( MessageParts
                .User( Prompt )
            )
            .Tools( ToolParts
                .Add( Tool.Beta.CreateToolSearchToolRegex20251119 )
                .Add( Tool.CreateToolCustom
                   .Name( 'get_weather' )
                   .Description( 'Get the weather at a specific location' )
                   .InputSchema( GetWeather )
                   .DeferLoading(True)
                )
                .Add( Tool.CreateToolCustom
                   .Name( 'search_files' )
                   .Description( 'Search through files in the workspace' )
                   .InputSchema( GetSearchFiles )
                   .DeferLoading(True)
                )
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

    // Synchronous example
  //  var Value := Client.Chat.Create(Payload);
  //
  //  try
  //    Display(TutorialHub, Value);
  //  finally
  //    Value.Free;
  //  end;
  ```

>[!NOTE]
> Mark tools for on-demand loading by adding `defer_loading: true`

- JSON Result (Excerpt)

  ```json
  "content": [
        {
            "type": "text",
            "text": "\n\nLet me search for weather-related tools that might help answer your question."
        },
        {
            "type": "server_tool_use",
            "id": "srvtoolu_01GXmqR8LS8kMzaePR2Xf7nu",
            "name": "tool_search_tool_regex",
            "input": {
                "pattern": "weather"
            },
            "caller": {
                "type": "direct"
            }
        },
        {
            "type": "tool_search_tool_result",
            "tool_use_id": "srvtoolu_01GXmqR8LS8kMzaePR2Xf7nu",
            "content": {
                "type": "tool_search_tool_search_result",
                "tool_references": [
                    {
                        "type": "tool_reference",
                        "tool_name": "get_weather"
                    }
                ]
            }
        },
        {
            "type": "text",
            "text": "I found a weather tool. Let me get the weather for San Francisco."
        },
        {
            "type": "tool_use",
            "id": "toolu_016XWJ2sVLy8jGh6qSisDFLg",
            "name": "get_weather",
            "input": {
                "location": "San Francisco"
            },
            "caller": {
                "type": "direct"
            }
        }
    ]
  ```

### Tool definition

The tool search tool has two variants:

```json
{
  "type": "tool_search_tool_regex_20251119",
  "name": "tool_search_tool_regex"
}
```

The regex-based tool search does **not** interpret queries as plain text.
Search inputs are evaluated strictly as **Python regular expressions** using `re.search()` semantics.

This means the query must be written to match **tool names or descriptions**, not to express intent in natural language.

Typical examples include:
- simple substring matching (e.g. matching any tool containing a given keyword)
- pattern-based matching using wildcards
- logical alternatives combined with regex OR operators
- optional case-insensitive matching via inline flags

The search engine enforces a hard limit on query size: **200 characters maximum**.

<br>

```json
{
  "type": "tool_search_tool_bm25_20251119",
  "name": "tool_search_tool_bm25"
}
```

In the BM25 variant, the search query is expressed in **plain natural language**.
The model formulates a semantic description of the desired capability rather than a pattern to match.

The search mechanism interprets the query as intent, ranking tools based on textual relevance across their names and descriptions.

Unlike the regex-based approach, no explicit matching rules or patterns are required; the emphasis is on **meaning**, not structure.