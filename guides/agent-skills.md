# Skills [beta]

**Agent Skills** extend Claude’s capabilities through packaged, executable capabilities ***combining instructions, scripts, and resources***, loaded and executed on demand during Messages requests.

They are designed to support document generation, data processing, and domain-specific workflows, while keeping the client-side API surface minimal.

- [Overview](#overview)
- [Execution model](#execution-model)
- [Minimal orchestration example](#minimal-orchestration-example)
- [Key constraints & limits](#key-constraints--limits)
- [References](#references)
- [Custom Skill lifecycle and API details](agent-skills-custom.md)

___

<br>

## Overview

**Agent Skills** can be used from two sources:
- **Anthropic-managed Skills** (e.g. pptx, xlsx, docx, pdf) <br>
  Pre-built, versioned, and maintained by Anthropic.
- **Custom Skills** <br>
  Workspace-scoped Skills uploaded and managed via the Skills API.

This feature is beta-only and requires explicit beta headers:
- `skills-2025-10-02` <br>
  Enables Skill discovery and attachment via container.skills.
- `code-execution-2025-08-25` <br>
  Enables the execution environment required to run Skills.

In practice, Skills are almost always used with both betas enabled in Messages requests. <br>
When Skills generate files, the Files API is additionally required to retrieve outputs.

<br>

## Execution model

**Skills** integrate with the Messages API through the execution container.

At a high level:
1. **Skill metadata exposure** <br>
   Claude sees each Skill’s name and description to determine relevance.
2. **Automatic selection** <br>
   Claude decides when a Skill should be used based on the user request.
3. **Progressive disclosure** <br>
   Full Skill instructions and files are loaded only when needed.
4. **Execution** <br>
   Skill code runs inside the code execution environment.
5. **Outputs** <br>
   Generated files are returned as file_id references and retrieved via the Files API.

Anthropic-managed and custom Skills follow the exact same execution path. <br>
They differ only in origin and lifecycle management, not in runtime behavior.

<br>

## Minimal orchestration example

The example below illustrates the minimal orchestration pattern:
- enable required betas
- attach one or more Skills to the execution container
- send a standard Messages request

```pascal

```

This is sufficient for:
- document generation (Excel, PowerPoint, Word, PDF)
- custom domain workflows
- multi-Skill composition

No additional orchestration logic is required on the client side.

<br>

## Key constraints & limits

Before using Skills in production, keep in mind:
- Maximum 8 Skills per request
- Skills run only inside the code execution environment
- Generated files must be downloaded via the Files API
- Adding or removing Skills invalidates prompt caches
- Skills are workspace-scoped and do not sync across surfaces
- Beta features are not covered by Zero Data Retention (ZDR)

Skills should be kept narrow and purposeful to preserve reliable trigger behavior.

<br>

## References

- Using Skills with the API
- Agent Skills overview
- Best practices for authoring Skills
- Code execution tool documentation
- Custom Skill lifecycle and API details: see [agent-skills-custom](agent-skills-custom.md)
