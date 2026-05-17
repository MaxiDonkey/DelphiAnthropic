---
name: delphi-uses-graph
description: Analyzes a Delphi / Object Pascal codebase to extract unit-level `uses` dependencies. Use when the user uploads a zip / archive of a Delphi project (or a folder of .pas / .dpr / .dpk files) and asks for a dependency graph, architecture map, cycle detection, fan-in / fan-out coupling analysis, or wants to know how units relate. Produces a Mermaid diagram, a Graphviz DOT file, an SVG when graphviz is available, a JSON dump of the parsed graph, and a markdown report listing cycles, hotspots and orphan units.
---

# Delphi uses-graph skill

This skill builds a directed graph of `uses` dependencies between Delphi /
Object Pascal units, computes coupling metrics, and detects circular
dependencies.

## When to use this skill

- The user uploads an archive (`.zip`, `.tar`, `.tar.gz`) containing a Delphi
  project, or a folder of `.pas` / `.dpr` / `.dpk` sources, and asks for an
  architecture map or a dependency graph.
- The user asks "which unit depends on what?", "are there cycles?",
  "what's the fan-in / fan-out?" against a Delphi codebase.
- The user wants a visual artifact (Mermaid embed, SVG, DOT) summarizing
  unit-level coupling.

Do **not** use this skill for non-Pascal codebases.

## Inputs

The skill expects exactly one of:

1. A `.zip` / `.tar` / `.tar.gz` archive of a Delphi project (preferred).
2. A directory already containing `.pas`, `.dpr`, `.dpk` files.

If the user pastes raw Pascal source instead, persist it to one or more
temporary `.pas` files first, then point `--input` at their parent directory.

## Workflow

1. Locate the input archive or directory in the working area.
2. Run `scripts/tool.py` with `--input` pointing at it and `--output`
   pointing at a fresh subfolder of the working area.
3. Read the generated `report.md` and summarize findings to the user:
   number of parsed units, top fan-in / fan-out, cycles, orphan units.
4. Attach the produced artifacts (Mermaid, DOT, SVG when present, JSON,
   report) to the response. Embed `uses-graph.mmd` inline in the answer so
   the user sees the diagram rendered.

## Quick start

```bash
python scripts/tool.py \
    --input  /path/to/project.zip \
    --output /path/to/out
```

Useful flags (full list in `reference.md`):

- `--scope {all,interface,implementation}` — which `uses` sections to
  consider. Default `all`.
- `--ignore-prefix LIST` — drop edges whose target starts with one of these
  comma-separated prefixes, case-insensitive. Default
  `System,Winapi,Vcl,FMX,Data,Web,REST,IdGlobal`. Pass `""` to keep RTL/VCL.
- `--max-label N` — truncate node labels in the Mermaid output (default 40).
- `--include-orphans` — keep units that have no inbound and no outbound
  edges in the graph.

## Outputs (in `--output`)

| File                | Purpose                                                                                  |
| ------------------- | ---------------------------------------------------------------------------------------- |
| `dependencies.json` | Raw graph: `{ unit: { defined_in, interface_uses, implementation_uses } }`               |
| `uses-graph.mmd`    | Mermaid `graph LR` ready to embed in a markdown answer                                   |
| `uses-graph.dot`    | Graphviz DOT source                                                                      |
| `uses-graph.svg`    | SVG rendering — only when the `dot` binary is available on `PATH`                        |
| `report.md`         | Human-readable summary: counts, top hotspots, cycles, orphans                            |

## Notes

- RTL / VCL / FMX targets are filtered by default to keep the diagram
  readable. The user's own units are never filtered.
- Cycle detection uses Tarjan's strongly connected components on the
  directed graph (edge `A -> B` means *A uses B*). Self-loops are reported
  separately.
- Pascal `uses` syntax has several edge cases (dotted names, `in '<path>'`
  aliases, conditional defines, three comment styles). Read `reference.md`
  before changing the parser.
