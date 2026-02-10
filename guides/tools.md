# Tools

Tools are the mechanism that allows Claude to go beyond text generation and take action, by orchestrating external functions in a controlled, typed, and conversation-integrated way.

- [Using tools with Claude](#using-tools-with-claude)
- [How tool use works](#how-tool-use-works)
- [How to implement tool use](#how-to-implement-tool-use)

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