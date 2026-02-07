# Thinking

Reasoning refers to the model’s ability to engage in internal analysis of varying depth in order to improve the quality of its responses to complex tasks.

- [Extended Reasoning](#extended-reasoning)
- [Adaptive Reasoning](#adaptive-reasoning)
- [Effort](#effort)
- [Quick Selection Guide](#quick-selection-guide)
- [Practical Notes](#practical-notes)

___

## Extended Reasoning

***Extended reasoning*** allows the model to perform explicit internal reasoning before producing its final answer.
Technically, this relies on the activation of thinking content blocks in which the model analyzes the problem and then incorporates the resulting insights into the textual output.
This reasoning is ***bounded by a token budget***, defined in advance, which limits the maximum depth of analysis. Higher budgets allow for more thorough reasoning, at the cost of increased token usage. On recent models (Opus 4.6), this mode is still supported but is now ***deprecated***.

<br>

### Extended Reasoning Mechanism

When extended reasoning is enabled, the model enters a dedicated internal analysis phase that is separate from the generation of the final response. This phase consists of specific segments where the reasoning process is developed and consolidated.

The conclusions derived from this analysis are then used to produce the user-facing output. From an API perspective, the response is structured in two successive parts: a section representing the internal reasoning, followed by the actual textual response.


```json
{
  "content": [
    {
      "type": "thinking",
      "thinking": "Let me analyze this step by step...",
      "signature": "WaUjzkypQ2mUEVM36O2TxuC06KN8xyfbJwyem2dw3URve/op91XWHOEBLLqIOMfFG/UvLEczmEsUjavL...."
    },
    {
      "type": "text",
      "text": "Based on my analysis..."
    }
  ]
}
```

See [official documentation](https://platform.claude.com/docs/en/build-with-claude/extended-thinking)

<br>

### How to use extended thinking

- JSON Payload creation - extended reasoning 

  ```pascal
    var ModelName := 'claude-sonnet-4-5';
    var MaxTokens := 16000;
    var BudgetTokens := 10000;
    var Prompt := 'Are there an infinite number of prime numbers such that n mod 4 == 3?';

    //JSON payload generation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Thinking( CreateThinkingConfig('enabled')
                .BudgetTokens(BudgetTokens)
            )
            .Messages( MessageParts
                .User( Prompt )
            );
      end;
  ```

- JSON Payload generated

  ```json
  {
        "model": "claude-sonnet-4-5",
        "max_tokens": 16000,
        "thinking": {
            "type": "enabled",
            "budget_tokens": 10000
        },
        "messages": [
            {
                "role": "user",
                "content": "Are there an infinite number of prime numbers such that n mod 4 == 3?"
            }
        ]
  }
  ```  

<br>

## Adaptive Reasoning

***Adaptive reasoning*** generalizes and improves extended reasoning by removing the need to manually specify a token budget.
The model decides ***dynamically*** whether to reason and how much, based on the actual complexity of the request.
This mode is now the ***recommended approach*** on ***Claude Opus 4.6***: it delivers better overall performance, avoids over- or under-allocation of reasoning, and enables ***interleaved reasoning*** with tool calls, which is particularly effective for agentic workflows.

<br>

### How to use adaptive thinking

- JSON Payload creation - adaptive reasoning 

  ```pascal
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 16000;
    var Prompt := 'Explain why the sum of two even numbers is always even.';

    //JSON payload generation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Thinking( CreateThinkingConfig('adaptive') )
            .Messages( MessageParts
                .User( Prompt )
            );
      end;
  ```

- JSON Payload generated

  ```json
  {
      "model": "claude-opus-4-6",
      "max_tokens": 16000,
      "thinking": {
          "type": "adaptive"
      },
      "messages": [
          {
              "role": "user",
              "content": "Explain why the sum of two even numbers is always even."
          }
      ]
  }
  ```

<br>

>[!NOTE]
>Prompt caching 
>- See [Official documentation](https://platform.claude.com/docs/en/build-with-claude/adaptive-thinking#prompt-caching)

<br>

## Effort

#### The ***effort*** parameter is a ***global control over token expenditure***, affecting:
- the depth of reasoning,
- the length and richness of responses,
- the number and complexity of tool calls.

#### Unlike a strict token budget, effort is a ***behavioral signal***:
- *low* prioritizes speed and cost,
- *medium* seeks a balance,
- *high* (default) maximizes quality,
- *max* (Opus 4.6 only) removes all capacity constraints.

On `Opus 4.6`, **effort** becomes the ***primary control lever*** for reasoning when combined with adaptive reasoning, progressively replacing explicit reasoning token budgets.

<br>

### How to use adaptive thinking with Effort

- JSON Payload creation - adaptive reasoning 

  ```pascal
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 16000;
    var Prompt := 'Can we find accumulation points in a discrete topology?';

    //JSON payload generation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Thinking( CreateThinkingConfig('adaptive') )
            .OutputConfig( CreateOutputConfig
                .Effort('max')
            )
            .Messages( MessageParts
                .User( Prompt )
            );
      end;
  ```

- JSON Payload generated

  ```json
  {
      "model": "claude-opus-4-6",
      "max_tokens": 16000,
      "thinking": {
          "type": "adaptive"
      },
      "output_config": {
          "effort": "max"
      },
      "messages": [
          {
              "role": "user",
              "content": "Can we find accumulation points in a discrete topology?"
          }
      ]
  }
  ```

See [Official Documentation](https://platform.claude.com/docs/en/build-with-claude/effort)

<br>

## Quick Selection Guide

Choosing the right reasoning mode for your use case

- **Extended Reasoning (deprecated)**
  Use only for:
  - backward compatibility with existing pipelines,
  - controlled experiments where explicit reasoning traceability is required.
  - High cost, manual budget management, not recommended for new systems.
- **Adaptive Reasoning (recommended)**
  Default choice for:
  - tasks with variable complexity,
  - agentic workflows with tool calls,
  - reducing over- or under-reasoning.
    Dynamic allocation, stronger multi-turn robustness.
- **Adaptive Reasoning + Effort**
  - `effort=low`: simple Q&A, strict latency or cost constraints.
  - `effort=medium`: general-purpose assistants, standard interactive use.
  - `effort=high` (default): serious analytical tasks.
  - `effort=max` (Opus 4.6 only): research, mathematics, complex planning, exhaustive exploration.

<br>

## Practical Notes

**Implementation best practices**
- **Avoid over-constraining reasoning**
  Do not combine explicit token budgets with behavioral signals (effort): let the model arbitrate.
- **Prefer adaptive reasoning in production**
  Better generational stability, less manual tuning, more predictable behavior under load.
- **Extended reasoning ≠ visible quality**
  More internal tokens do not necessarily translate into better user-facing answers.
- **Effort is a global control lever**
  It simultaneously affects:
  - reasoning depth,
  - output length,
  - frequency and complexity of tool calls.
- **Prompt caching**
  Particularly effective with adaptive reasoning on long or recurring contexts.
- **API design guideline**
  For Opus 4.6:
  > Adaptive reasoning combined with a well-calibrated effort setting cleanly replaces manual reasoning management.