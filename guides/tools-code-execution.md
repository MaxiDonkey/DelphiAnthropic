# Code execution tool [beta]

Code execution tools are tooled capabilities exposed to a model to execute operations (computation, shell commands, file manipulation) within a sandboxed environment, in order to produce concrete and reproducible results.

- [API Overview](#api-overview)
- [How to Use the Tool](#how-to-use-the-tool)
- [Using and Reusing Containers](#using-and-reusing-containers)
- [Recommendations (Design and Architecture Focus)](#recommendations-design-and-architecture-focus)
- [Quick Selection Guide](#quick-selection-guide)
- [Practical Notes](#practical-notes)

___

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

Ask Claude to check system information and install packages:

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

## Using and Reusing Containers


<br>

## Quick Selection Guide


<br>

## Practical Notes


