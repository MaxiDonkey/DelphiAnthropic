# MCP connector [beta]

The MCP connector is a simple way to give models access to remote tools, without embedding a full MCP client in your application.

- [What is the MCP Connector?](#what-is-the-mcp-connector)
- [What is it used for?](#what-is-it-used-for)
- [How it works](#how-it-works)
- [Minimal example](#minimal-example)
- [Key things to know](#Key-things-to-know)
- [Remote MCP servers](#remote-mcp-servers)

___

<br>

## What is the MCP Connector?

The **Model Context Protocol (MCP) connector** allows an AI model to use **tools exposed by a remote MCP server**, directly through the *Messages API*.

In practice, this means the model can call functions hosted on an MCP server (search, computation, access to external systems) **without requiring the client application to implement a full MCP client**.

The MCP connector supports **tool calls only**. It is not an agent framework, an orchestrator, or a general-purpose runtime.

<br>

## What is it used for?

The MCP connector is designed to **externalize model capabilities** into specialized remote services while keeping a simple and uniform API.

It is useful when:
- tools are hosted **remotely** and shared
- multiple models or applications need access to the same capabilities
- you want to minimize business logic on the client side

It is not a good fit when:
- tools must run locally
- you need other MCP features (prompts, resources)
- you require fine-grained control over the MCP runtime

<br>

## How it works

The connector is built around two elements:
- **MCP Server** <br>
  A remote service that exposes tools over a public HTTP API.
- **Toolset** <br>
  A configuration that defines which tools from the server are visible to the model and how they are configured.

>[!IMPORTANT]
>Each MCP server must be associated with exactly one toolset.

Request flow:
1. the model receives the description of the allowed tools
2. it decides to call a tool
3. the MCP server executes the tool and returns the result

<br>

## Minimal example

```pascal
  var ModelName := 'claude-opus-4-7';
  var MaxTokens := 1024;
  var SystemPrompt := 'Today is ''' + FormatDateTime('dd"u"mmmm"t"yyyy', Date) + ''' (' + FormatDateTime('yyyy-mm-dd', Date) + ').';
  var Prompt := 'What is the weather like in New York today?';

  var McpUrl := 'https://gemini-api-demos.uc.r.appspot.com/mcp';
  var McpName := 'weather_service';
  var McpToken := '';

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Beta(['mcp-client-2025-11-20'])
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .System( SystemPrompt )
          .Messages( MessageParts
              .User( Prompt )
          )
          .McpServers( MCPServerParts
               .Add( MCPServer.CreateMCPServer
                   .Url( McpUrl )
                   .Name( McpName )
                   .AuthorizationToken( McpToken )
               )
          )
          .Tools( ToolParts
              .Add( Tool.Beta.CreateMCPToolset
                  .McpServerName( McpName )
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

<br>

- JSON Result (exerpt)

```json
"content": [
        {
            "type": "text",
            "text": "\n\nLet me check the current weather in New York for you!"
        },
        {
            "type": "mcp_tool_use",
            "id": "mcptoolu_014nxMAWGJJed9tkKMWAjPNz",
            "name": "get_weather",
            "input": {
                "location": "New York",
                "startDate": "2026-02-18",
                "endDate": "2026-02-18"
            },
            "server_name": "weather_service"
        },
        {
            "type": "mcp_tool_result",
            "tool_use_id": "mcptoolu_014nxMAWGJJed9tkKMWAjPNz",
            "is_error": false,
            "content": [
                {
                    "type": "text",
                    "text": "{\"2026-02-18T00:00\":\"5.1°C\",\"2026-02-18T01:00\":\"3.8°C\",\"2026-02-18T02:00\":\"3°C\",\"2026-02-18T03:00\":\"2.6°C\",\"2026-02-18T04:00\":\"2.7°C\",\"2026-02-18T05:00\":\"2°C\",\"2026-02-18T06:00\":\"1.4°C\",\"2026-02-18T07:00\":\"0.9°C\",\"2026-02-18T08:00\":\"1°C\",\"2026-02-18T09:00\":\"1.6°C\",\"2026-02-18T10:00\":\"1.8°C\",\"2026-02-18T11:00\":\"2.6°C\",\"2026-02-18T12:00\":\"2.1°C\",\"2026-02-18T13:00\":\"2.6°C\",\"2026-02-18T14:00\":\"3.2°C\",\"2026-02-18T15:00\":\"3.3°C\",\"2026-02-18T16:00\":\"4.2°C\",\"2026-02-18T17:00\":\"5.2°C\",\"2026-02-18T18:00\":\"6.7°C\",\"2026-02-18T19:00\":\"7.9°C\",\"2026-02-18T20:00\":\"8.6°C\",\"2026-02-18T21:00\":\"8°C\",\"2026-02-18T22:00\":\"6.8°C\",\"2026-02-18T23:00\":\"5.8°C\"}"
                }
            ]
        },
        {
            "type": "text",
            "text": "Here's the weather forecast for **New York** today, **February 18, 2026**:\n\n🌡️ **Temperature Summary:**\n- **Low:** 0.9°C (33.6°F) — around 7:00 AM\n- **High:** 8.6°C (47.5°F) — around 8:00 PM\n- **Current trend:** Cool morning, gradually warming through the afternoon and evening\n\n📋 **Hourly Breakdown:**\n\n| Time of Day | Temperature |\n|---|---|\n| 🌙 Overnight (12–6 AM) | 5.1°C → 1.4°C (cooling) |\n| 🌅 Morning (7–11 AM) | 0.9°C → 2.6°C (slowly warming) |\n| ☀️ Afternoon (12–4 PM) | 2.1°C → 4.2°C (mild) |\n| 🌆 Evening (5–8 PM) | 5.2°C → 8.6°C (warmest) |\n| 🌙 Night (9–11 PM) | 8.0°C → 5.8°C (cooling off) |\n\nOverall, it's a **chilly winter day** in New York with temperatures hovering between about **1°C and 9°C (33–47°F)**. You'll want to dress warmly with layers! 🧥\n\nWould you like me to check the rain forecast or anything else?"
        }
    ]
```

<br>

## Key things to know

Before going further, keep in mind:
- the MCP connector requires a specific beta header
- only **public HTTP MCP servers** are supported
- only **tool usage** is exposed
- OAuth authentication is handled by the client; the connector only forwards the token
- tool configuration now lives in the toolset, not in the server definition (this is a breaking change from older versions)

<br>

## Remote MCP servers

See [official documentation](https://platform.claude.com/docs/en/agents-and-tools/remote-mcp-servers).