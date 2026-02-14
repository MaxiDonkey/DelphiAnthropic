# Beta Tools

At Anthropic, “beta” tools are features that are accessed through explicit beta headers, are versioned independently from the core API, and whose availability and behavior depend on the combination of model version, tool version, and enabled beta flags.


- [Introduction](#introduction)
- [Code execution](tools-code-execution.md#code-execution-tool-beta)

___

## Introduction

Beta tools are opt-in API features gated by beta headers and exposed through versioned tool interfaces. They allow Anthropic to ship new capabilities while retaining strict control over compatibility, rollout, and model–tool alignment. As a result, beta tools are version-scoped and may change independently of the core API.

Beta tools are not enabled by default and must be explicitly activated using the appropriate beta headers. Tool versions are validated against specific model versions and are not guaranteed to remain compatible across models or over time. Beta tools must always be referenced using explicit, versioned tool identifiers.

When multiple beta features are combined, each combination should be treated as an independent integration surface and tested accordingly. Some beta tools also require client-side implementations or execution environments, which are the responsibility of the application developer.

<br>