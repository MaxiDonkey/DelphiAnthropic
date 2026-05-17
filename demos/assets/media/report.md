# Delphi uses-graph report

- Parsed units: **9**
- Edges (scope = `all`): **12**
- Cycles (size > 1): **1**
- Self-loops: **0**
- Orphan units (no inbound and no outbound): **0**
- SVG rendered: **no (graphviz `dot` not found on PATH)**

## Top fan-in (most depended-upon)

| Unit | fan-in |
|---|---|
| `Demo.Anthropic.AsyncUtils` | 2 |
| `Demo.Anthropic.Context` | 2 |
| `Demo.Anthropic.Services` | 2 |
| `Demo.Anthropic.FileIds` | 1 |
| `Demo.Anthropic.Helpers` | 1 |
| `Demo.Anthropic.JsonResponse.Helper` | 1 |
| `Main` | 1 |
| `VCL.WVPythia.Services` | 1 |
| `VCL_Anthropic` | 0 |

## Top fan-out (most dependencies)

| Unit | fan-out |
|---|---|
| `Demo.Anthropic.Services` | 5 |
| `Main` | 4 |
| `VCL.WVPythia.Services` | 2 |
| `VCL_Anthropic` | 1 |
| `Demo.Anthropic.AsyncUtils` | 0 |
| `Demo.Anthropic.Context` | 0 |
| `Demo.Anthropic.FileIds` | 0 |
| `Demo.Anthropic.Helpers` | 0 |
| `Demo.Anthropic.JsonResponse.Helper` | 0 |

## Cycles

1. Main → VCL.WVPythia.Services → Main
