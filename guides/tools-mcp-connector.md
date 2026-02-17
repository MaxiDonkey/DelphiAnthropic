# MCP connector [beta]

The MCP connector is a simple way to give models access to remote tools, without embedding a full MCP client in your application.

- [What is the MCP Connector?](#what-is-the-mcp-connector)
- [What is it used for?](#what-is-it-used-for)
- [How it works](#how-it-works)
- [Minimal example](#minimal-example)
- [Key things to know](#Key-things-to-know)

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

<br>

## Key things to know

Before going further, keep in mind:
- the MCP connector requires a specific beta header
- only **public HTTP MCP servers** are supported
- only **tool usage** is exposed
- OAuth authentication is handled by the client; the connector only forwards the token
- tool configuration now lives in the toolset, not in the server definition (this is a breaking change from older versions)
