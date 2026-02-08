# Fast mode (research preview)

Fast mode is an execution option that accelerates response generation for Claude Opus 4.6 without changing its capabilities, by relying solely on a different inference configuration.

- [How fast mode works](#how-fast-mode-works)
- [Checking which speed was used](#checking-which-speed-was-used)
- [Quick Selection Guide](#quick-selection-guide)
- [Practical Notes](#practical-notes)

___

## How fast mode works

Fast mode runs the same model with the same weights, but uses an inference setup optimized to increase output tokens per second (OTPS), up to approximately 2.5× compared to standard mode.
There is ***no impact on model intelligence or behavior***: the speedup applies only to generation after the first token (not to time to first token, TTFT).
In practice, fast mode is well suited for ***high-volume text generation or workloads*** sensitive to ***overall latency***.


- JSON Payload creation
  ```pascal
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 4096;
    var SpeedValue := 'fast';
    var Prompt := 'Refactor this module to use dependency injection';

    //JSON payload generation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Beta(['fast-mode-2026-02-01'])
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Speed(SpeedValue)
            .Messages( MessageParts
                .User( Prompt )
            );
      end;
  ```

<br>

## Checking which speed was used

The speed actually used is explicitly reported in the response via the `usage.speed` field, which is either `"fast"` or `"standard"`.
This makes it possible to verify post hoc whether a request benefited from fast mode (or fell back to standard mode, for example after a retry), and to ***accurately correlate performance and cost*** in application-level metrics.

```json
{
  "id": "msg_01XFDUDYJgAACzvnptvVoYEL",
  "type": "message",
  "role": "assistant",
  ...
  "usage": {
    "input_tokens": 523,
    "output_tokens": 1842,
    "speed": "fast"
  }
}
```

<br>

## Quick Selection Guide

- Use ***fast mode*** when ***generation throughput*** is the primary concern (long outputs, agents, iterative loops, dense streaming) and the pricing premium is acceptable.
- Prefer ***standard mode*** for short or infrequent requests, or when ***marginal cost per token*** is the dominant constraint.
- Fast mode is therefore an ***operational choice***, not a qualitative one.

<br>

## Practical Notes

- Fast mode is strictly limited to ***Claude Opus 4.6*** and requires the corresponding ***beta flag*** to be enabled.
- Fast mode and standard mode do ***not share the prompt cache***: switching speeds results in a cache miss, even with identical prompts.
- ***Fallback to standard mode*** must be handled explicitly client-side when a `429` error occurs, with a direct impact on ***latency*** and cost due to cache invalidation.
- ***Rate limits are dedicated*** to fast mode and exposed through specific response headers, enabling fine-grained capacity management.
- The `usage.speed` field is the ***authoritative source*** for auditing actual performance and cost behavior.
