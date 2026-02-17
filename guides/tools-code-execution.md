# Code execution tool [beta]

The Code Execution tool is a server-side capability exposed to the model to execute operations (computation, shell commands, file manipulation) within a sandboxed environment, in order to produce concrete and reproducible results.

- [API Overview](#api-overview)
- [How to Use the Tool](#how-to-use-the-tool)
- [Using and Reusing Containers](#using-and-reusing-containers)
- [Recommendations (Design and Architecture Focus)](#recommendations-design-and-architecture-focus)
- [Quick Selection Guide](#quick-selection-guide)
- [Practical Notes](#practical-notes)

___

>[!NOTE]
> This tutorial presents the minimal usage pattern for clarity. More advanced patterns (multiple or chained tool calls) can be built on top of this foundation.

<br>

## API Overview

### Objective

Enable a model to execute code and system operations inside an ***isolated container*** (sandbox), and return:
- execution outputs (`stdout`, `stderr`, return codes),
- produced artifacts (files, visualizations, reports).

### Activation

- Add the **beta header**: `code-execution-2025-08-25`
- Declare the tool in the request:
  
  ```pascal
    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Beta(['code-execution-2025-08-25'])
            ...
            .Tools( ToolParts
                .Add( Tool.Beta.CreateCodeExecutionTool20250825 )
            )
           ...
      end;   
  ```
- JSON Payload content
  
  ```json
  { "type": "code_execution_20250825", "name": "code_execution" }
  ```  
  
  Added the `code-execution-2025-08-25` header to the HTTP request.

### Compatibility

- Available on **Claude 4.x / 4.5 / 4.6** models (Opus, Sonnet, Haiku)
- Same tool version across all supported models

### Core capabilities (`code_execution_20250825`) 

- Execution of **Bash commands** (system inspection, scripting, file management)
- **File creation, viewing, and modification**
- Orchestration of **multi-step workflows**, within a single turn or across multiple requests

### Runtime constraints

- Isolated Linux container
- Python 3.11.x
- Bounded resources (CPU, RAM, disk)
- **No internet access**
- Fully sandboxed environment

<br>

## How to Use the Tool

### Minimal usage pattern

#### 1. Send a Messages API request with:

- `.beta(['code-execution-2025-08-25'])`
- `.tools('[{ "type": "code_execution_20250825", "name": "code_execution" }]')`

#### 2. The model decides whether to invoke the tool and may chain multiple actions:

- shell commands,
- file operations.

#### 3. The response contains typed result blocks:

- Bash execution results (`stdout`, `stderr`, `return_code`),
- file operation results (content, diffs),
- references to produced files (artifacts).

<br>

### Key considerations

- **Implicit capabilities** <br>
  Declaring code_execution_20250825 automatically enables Bash execution and file editing.

- **User-provided files** <br>
  To analyze CSV, Excel, images, etc.:
  - upload files via the Files API,
  - reference the file_id in the message,
  - include the Files API beta header when required.
- **Result parsing** <br>
  Programmatic consumers must explicitly distinguish between:
  - execution results,
  - file operations,
  - downloadable artifacts.

<br>

### Basic example 

Ask Claude to check system information:

```pascal
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 4096;
  var Prompt := 'Check the Python version and list installed packages';

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Beta(['code-execution-2025-08-25'])
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Messages( MessageParts
              .User( Prompt)
          )
          .Tools( ToolParts
              .Add( Tool.Beta.CreateCodeExecutionTool20250825 )
          );
    end;

  // Asynchronous creation (promise-based)
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

See [official documentation](https://platform.claude.com/docs/en/agents-and-tools/tool-use/code-execution-tool#create-and-edit-files-directly)

### Result

```text
I'll check the Python version and list the installed packages simultaneously.
Here's a summary of the environment:

### Python Version
- **Python 3.11.13**

### Installed Packages (170+ packages)
Here are some notable ones grouped by category:
```

| Category | Packages |
|---|---|
| **Data Science & ML** | `numpy`, `pandas`, `scipy`, `scikit-learn`, `xgboost`, `shap`, `statsmodels`, `numba` |
| **Visualization** | `matplotlib`, `seaborn`, `bokeh`, `altair`, `holoviews`, `hvplot`, `wordcloud` |
| **Image Processing** | `pillow`, `opencv-python-headless`, `scikit-image`, `imageio`, `ImageMagick` (CLI) |
| **PDF & Documents** | `PyMuPDF`, `pdfplumber`, `pdfminer.six`, `pypdf`, `pikepdf`, `pdf2image`, `reportlab`, `img2pdf` |
| **Office Files** | `openpyxl`, `xlrd`, `XlsxWriter`, `python-docx`, `python-pptx`, `tabula-py` |
| **Geospatial** | `geopandas`, `shapely`, `Fiona`, `pyproj` |
| **Web & Parsing** | `beautifulsoup4`, `requests`, `lxml`, `markdownify`, `markitdown` |
| **NLP** | `nltk`, `regex`, `tokenizers` |
| **Data Formats** | `pyarrow`, `h5py`, `PyYAML`, `jq` (CLI) |
| **AI/ML Runtime** | `onnxruntime`, `huggingface-hub` |
| **Utilities** | `tqdm`, `rich`, `click`, `networkx`, `sympy`, `graphviz` |

- JSON result (excerpt)

```json
"content": [
        {
            "type": "text",
            "text": "\n\nI'll check the Python version and list the installed packages simultaneously."
        },
        {
            "type": "server_tool_use",
            "id": "srvtoolu_012cyAPUZzNLiLzkTPnpnPyy",
            "name": "bash_code_execution",
            "input": {
                "command": "python3 --version"
            },
            "caller": {
                "type": "direct"
            }
        },
        {
            "type": "server_tool_use",
            "id": "srvtoolu_01BWPkHsvCLXc5zxry6W9jEn",
            "name": "bash_code_execution",
            "input": {
                "command": "pip list 2>\/dev\/null || pip3 list 2>\/dev\/null"
            },
            "caller": {
                "type": "direct"
            }
        },
        {
            "type": "bash_code_execution_tool_result",
            "tool_use_id": "srvtoolu_012cyAPUZzNLiLzkTPnpnPyy",
            "content": {
                "type": "bash_code_execution_result",
                "stdout": "Python 3.11.13\n",
                "stderr": "",
                "return_code": 0,
                "content": [
                ]
            }
        },
        {
            "type": "bash_code_execution_tool_result",
            "tool_use_id": "srvtoolu_01BWPkHsvCLXc5zxry6W9jEn",
            "content": {
                "type": "bash_code_execution_result",
                "stdout": "Package                   Version\n--- ... 3.18.1\n",
                "stderr": "",
                "return_code": 0,
                "content": [
                ]
            }
        },
        {
            "type": "text",
            "text": "Here's a summary of the environment:\n\n### ... learning, document processing, and visualization tasks!"
        }
    ],
    "container": {
        "id": "container_011CY7NJtwv7RuL8EKzCV5aU",
        "expires_at": "2026-02-14T05:57:17.692767Z"
    }
```
> ***This final JSON includes a `container` identifier, the notion of which is addressed in the following section.***

<br>

## Using and Reusing Containers

### Principle

Each execution runs inside a container. The same container can be reused across requests to preserve state (files, scripts, intermediate outputs).

### Mechanism

- First request: the API creates a container and returns a `container.id`
- Subsequent requests: pass this `container.id` to resume the same environment

### Benefits

- Multi-step pipelines (preprocessing → analysis → export)
- Reduced redundant uploads
- Iterative work on temporary codebases or datasets

### Limitations

- Containers expire after a retention period
- No network access
- Requires strict hygiene: naming, directory structure, explicit artifacts

### Basic example with container ID

```pascal
  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Beta(['code-execution-2025-08-25'])
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Messages( MessageParts
              .User( Prompt)
          )
          .Tools( ToolParts
              .Add( Tool.Beta.CreateCodeExecutionTool20250825 )
          )
          .Container('container_011CY7NJtwv7RuL8EKzCV5aU');  // <--- Add container ID
    end;
```

<br>

## Recommendations (Design and Architecture Focus)

### Architecture and contract

- Encapsulate tool usage behind a dedicated layer:
  - request construction (headers, tools),
  - response block parsing,
  - error and artifact handling.
- Explicitly version the tool contract (header + tool type) and plan for format evolution.

### State strategy

- Clearly separate:
  - stateless mode (ephemeral container),
  - stateful mode (reused container).
- Design workflows to be idempotent: container loss must not break the pipeline.

### Artifacts-first approach

- Prefer producing files (reports, plots, dumps) over returning large textual outputs.
- Standardize a directory structure such as:
  ```bash
  /workspace/<job_id>/
  ```
  along with a manifest listing generated outputs.

### Observability and governance

- Systematically log:
  - executed commands,
  - return codes,
  - stderr output,
  - produced file paths.
- Enforce strict policies for:
  - secrets (never store keys in clear text),
  - Bash command surface,
  - data size and volume limits.

### Cost and latency

- Reuse containers only when retained state provides tangible value.
- Minimize uploads and favor compact data formats.
- Chunk long-running jobs and checkpoint progress via intermediate files.

<br>

## Quick Selection Guide

**Objective: quickly select the appropriate configuration for a given use case.**

- **One-off computation / demo / notebook-style (no state)** <br>
  → Single request, ephemeral container <br>
  Typical cases: quick statistics, inspection, simple transformations <br>

- **Multi-step workflow (analysis → visualization → export)** <br>
  → Reused container + artifacts <br>
  Typical cases: processing pipelines, report generation
- **User file analysis (CSV, Excel, images)** <br>
  → Files API + file_id (+ Files API beta header if required) <br>
  Typical cases: datasets, documents, images to be analyzed
- **System-level orchestration (bash, scripts, directory layout)** <br>
  → Structured Bash usage + directory conventions + logging <br>
  Typical cases: environment preparation, local batch jobs, temporary project generation

<br>

## Practical Notes

**Request-side contract**
- Enable `code-execution-2025-08-25`
- Explicitly declare the `code_execution_20250825` tool

**Result handling**
- Parse separately:
  - Bash execution results,
  - file operation results,
  - artifacts.
- Treat any `return_code ≠ 0` as a tool execution failure, regardless of generated explanation.

**File handling**
- Inputs: upload via Files API and reference by `file_id` (See [Files API](files-api.md#files-api))
- Outputs: retrieve artifacts via Files API (avoid passing large outputs through tokens)

**Container reuse**
- Capture the initial `container.id`
- Pass it in subsequent requests to preserve state
- Design workflows to be resumable in case of container expiration

**Runtime constraints**
- No internet access
- Bounded resources
- Explicitly handle: <br>
  `execution_time_exceeded`, `too_many_requests`, `container_expired`, `pause_turn`

<br>

>[!IMPORTANT]
> The model performs the reasoning, the container handles execution, and the application governs the process.
