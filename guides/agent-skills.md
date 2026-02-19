# Skills [beta]

**Agent Skills** extend Claude’s capabilities through packaged, executable capabilities units ***combining instructions, scripts, and resources***, loaded and executed on demand during Messages requests.

They are designed to support document generation, data processing, and domain-specific workflows, while keeping the client-side API surface minimal.

- [Overview](#overview)
- [Execution model](#execution-model)
- [Custom Skills (overview)](#custom-skills-overview)
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
   ***Skills are never invoked explicitly by the client; once attached to the container, Claude decides when (and if) they should be used.***
3. **Progressive disclosure** <br>
   Full Skill instructions and files are loaded only when needed.
4. **Execution** <br>
   Skill code runs inside the code execution environment.
5. **Outputs** <br>
   Generated files are returned as file_id references and retrieved via the Files API.

Anthropic-managed and custom Skills follow the exact same execution path. <br>
They differ only in origin and lifecycle management, not in runtime behavior.

<br>

## Custom Skills (overview)

In addition to Anthropic-managed Skills, you can create **custom Skills** to package domain-specific workflows, reusable procedures, or organization-specific logic.

A custom Skill is defined as a **directory-based bundle** containing:
- a mandatory `SKILL.md` file (metadata and instructions)
- optional scripts, references, and supporting resources

```text
my-skill/
├── SKILL.md
├── reference.md
└── scripts/
    └── tool.py
```

>[!IMPORTANT]
>The uploaded files must belong to a single top-level directory, and this directory must contain `SKILL.md` at its root. 

<br>

Once uploaded, custom Skills:
- behave exactly like Anthropic-managed Skills at runtime
- are attached and executed through `container.skills`
- can be composed with other Skills in the same request

The creation, versioning, and deletion of custom Skills are handled through the [Skills API](agent-skills-custom.md) and are documented separately.

<br>

## Minimal orchestration example

- [Delphi code](#delphi-code)
- [JSON Result](#json-result)
- [How to Use the Model Outputs](#how-to-use-the-model-outputs)

<br>

The example below illustrates the minimal orchestration pattern:
- enable required betas
- attach one or more Skills to the execution container
- send a standard Messages request

### Delphi code

```pascal
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 4096;
  var Prompt := 'Create an Excel file with a simple budget spreadsheet.';

  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Beta(['code-execution-2025-08-25', 'skills-2025-10-02'])
          .Model( ModelName )
          .MaxTokens( MaxTokens )
          .Container( CreateContainer
              .Skills( SkillParts
                  .Add( Skill.CreateSkill('anthropic')
                     .SkillId('xlsx')
                     .Version('latest')
                  )
              )
          )
          .Messages( MessageParts
              .User( ContentParts
                 .AddText( Prompt )
              )
          )
          .Tools( ToolParts
              .Add( Tool.Beta.CreateCodeExecutionTool20250825 )
          );
    end;

  // Set response delay for 10 min
  Client.HttpClient.ResponseTimeout := 600000;

  // Asynchronous example
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

This is sufficient for:
- document generation (Excel, PowerPoint, Word, PDF)
- custom domain workflows
- multi-Skill composition

No additional orchestration logic is required on the client side.

<br>

### JSON Result

```json
{
    "model": "claude-opus-4-6",
    "id": "msg_01546VF7Lh1tfwAKDXzkWnNd",
    "type": "message",
    "role": "assistant",
    "content": [
        {
            "type": "text",
            "text": "\n\nI'll start by reading the skill file for Excel spreadsheet creation."
        },
        {
            "type": "server_tool_use",
            "id": "srvtoolu_01SadFweDcLhMt6q68aJ3RfC",
            "name": "text_editor_code_execution",
            "input": {
                "command": "view",
                "path": "\/skills\/xlsx\/SKILL.md"
            },
            "caller": {
                "type": "direct"
            }
        },
        {
            "type": "text_editor_code_execution_tool_result",
            "tool_use_id": "srvtoolu_01SadFweDcLhMt6q68aJ3RfC",
            "content": {
                "type": "text_editor_code_execution_view_result",
                "file_type": "text",
                "content": "---\nname: xlsx\ndescription: \"Use this skill any time a spreadsheet file is the primary input or output. This means any task where the user wants to: open, read, edit, or fix an existing .xlsx, .xlsm, .csv, or .tsv file (e.g., adding columns, computing formulas, formatting, charting, cleaning messy data); create a new spreadsheet from scratch or from other data sources; or convert between tabular file formats. Trigger especially when the user references a spreadsheet file by name or path — even casually (like \\\"the xlsx in my downloads\\\") — and wants something done to it or produced from it. Also trigger for cleaning or restructuring messy tabular data files (malformed rows, misplaced headers, junk data) into proper spreadsheets. The deliverable must be a spreadsheet file. Do NOT trigger when the primary deliverable is a Word document, HTML report, standalone Python script, database pipeline, or Google Sheets API integration, even if tabular data is involved.\"\nlicense: Proprietary. LICENSE.txt has complete terms\n---\n\n# Requirements for Outputs\n\n## All Excel files\n\n### Professional Font\n- Use a consistent, professional font (e.g., Arial, Times New Roman) for all deliverables unless otherwise instructed by the user\n\n### Zero Formula Errors\n- Every Excel model MUST be delivered with ZERO formula errors (#REF!, #DIV\/0!, #VALUE!, #N\/A, #NAME?)\n\n### Preserve Existing Templates (when updating templates)\n- Study and EXACTLY match existing format, style, and conventions when modifying files\n- Never impose standardized formatting on files with established patterns\n- Existing template conventions ALWAYS override these guidelines\n\n## Financial models\n\n### Color Coding Standards\nUnless otherwise stated by the user or existing template\n\n#### Industry-Standard Color Conventions\n- **Blue text (RGB: 0,0,255)**: Hardcoded inputs, and numbers users will change for scenarios\n- **Black text (RGB: 0,0,0)**: ALL formulas and calculations\n- **Green text (RGB: 0,128,0)**: Links pulling from other worksheets within same workbook\n- **Red text (RGB: 255,0,0)**: External links to other files\n- **Yellow background (RGB: 255,255,0)**: Key assumptions needing attention or cells that need to be updated\n\n### Number Formatting Standards\n\n#### Required Format Rules\n- **Years**: Format as text strings (e.g., \"2024\" not \"2,024\")\n- **Currency**: Use $#,##0 format; ALWAYS specify units in headers (\"Revenue ($mm)\")\n- **Zeros**: Use number formatting to make all zeros \"-\", including percentages (e.g., \"$#,##0;($#,##0);-\")\n- **Percentages**: Default to 0.0% format (one decimal)\n- **Multiples**: Format as 0.0x for valuation multiples (EV\/EBITDA, P\/E)\n- **Negative numbers**: Use parentheses (123) not minus -123\n\n### Formula Construction Rules\n\n#### Assumptions Placement\n- Place ALL assumptions (growth rates, margins, multiples, etc.) in separate assumption cells\n- Use cell references instead of hardcoded values in formulas\n- Example: Use =B5*(1+$B$6) instead of =B5*1.05\n\n#### Formula Error Prevention\n- Verify all cell references are correct\n- Check for off-by-one errors in ranges\n- Ensure consistent formulas across all projection periods\n- Test with edge cases (zero values, negative numbers)\n- Verify no unintended circular references\n\n#### Documentation Requirements for Hardcodes\n- Comment or in cells beside (if end of table). Format: \"Source: [System\/Document], [Date], [Specific Reference], [URL if applicable]\"\n- Examples:\n  - \"Source: Company 10-K, FY2024, Page 45, Revenue Note, [SEC EDGAR URL]\"\n  - \"Source: Company 10-Q, Q2 2025, Exhibit 99.1, [SEC EDGAR URL]\"\n  - \"Source: Bloomberg Terminal, 8\/15\/2025, AAPL US Equity\"\n  - \"Source: FactSet, 8\/20\/2025, Consensus Estimates Screen\"\n\n# XLSX creation, editing, and analysis\n\n## Overview\n\nA user may ask you to create, edit, or analyze the contents of an .xlsx file. You have different tools and workflows available for different tasks.\n\n## Important Requirements\n\n**LibreOffice Required for Formula Recalculation**: You can assume LibreOffice is installed for recalculating formula values using the `scripts\/recalc.py` script. The script automatically configures LibreOffice on first run, including in sandboxed environments where Unix sockets are restricted (handled by `scripts\/office\/soffice.py`)\n\n## Reading and analyzing data\n\n### Data analysis with pandas\nFor data analysis, visualization, and basic operations, use **pandas** which provides powerful data manipulation capabilities:\n\n```python\nimport pandas as pd\n\n# Read Excel\ndf = pd.read_excel('file.xlsx')  # Default: first sheet\nall_sheets = pd.read_excel('file.xlsx', sheet_name=None)  # All sheets as dict\n\n# Analyze\ndf.head()      # Preview data\ndf.info()      # Column info\ndf.describe()  # Statistics\n\n# Write Excel\ndf.to_excel('output.xlsx', index=False)\n```\n\n## Excel File Workflows\n\n## CRITICAL: Use Formulas, Not Hardcoded Values\n\n**Always use Excel formulas instead of calculating values in Python and hardcoding them.** This ensures the spreadsheet remains dynamic and updateable.\n\n### ❌ WRONG - Hardcoding Calculated Values\n```python\n# Bad: Calculating in Python and hardcoding result\ntotal = df['Sales'].sum()\nsheet['B10'] = total  # Hardcodes 5000\n\n# Bad: Computing growth rate in Python\ngrowth = (df.iloc[-1]['Revenue'] - df.iloc[0]['Revenue']) \/ df.iloc[0]['Revenue']\nsheet['C5'] = growth  # Hardcodes 0.15\n\n# Bad: Python calculation for average\navg = sum(values) \/ len(values)\nsheet['D20'] = avg  # Hardcodes 42.5\n```\n\n### ✅ CORRECT - Using Excel Formulas\n```python\n# Good: Let Excel calculate the sum\nsheet['B10'] = '=SUM(B2:B9)'\n\n# Good: Growth rate as Excel formula\nsheet['C5'] = '=(C4-C2)\/C2'\n\n# Good: Average using Excel function\nsheet['D20'] = '=AVERAGE(D2:D19)'\n```\n\nThis applies to ALL calculations - totals, percentages, ratios, differences, etc. The spreadsheet should be able to recalculate when source data changes.\n\n## Common Workflow\n1. **Choose tool**: pandas for data, openpyxl for formulas\/formatting\n2. **Create\/Load**: Create new workbook or load existing file\n3. **Modify**: Add\/edit data, formulas, and formatting\n4. **Save**: Write to file\n5. **Recalculate formulas (MANDATORY IF USING FORMULAS)**: Use the scripts\/recalc.py script\n   ```bash\n   python scripts\/recalc.py output.xlsx\n   ```\n6. **Verify and fix any errors**: \n   - The script returns JSON with error details\n   - If `status` is `errors_found`, check `error_summary` for specific error types and locations\n   - Fix the identified errors and recalculate again\n   - Common errors to fix:\n     - `#REF!`: Invalid cell references\n     - `#DIV\/0!`: Division by zero\n     - `#VALUE!`: Wrong data type in formula\n     - `#NAME?`: Unrecognized formula name\n\n### Creating new Excel files\n\n```python\n# Using openpyxl for formulas and formatting\nfrom openpyxl import Workbook\nfrom openpyxl.styles import Font, PatternFill, Alignment\n\nwb = Workbook()\nsheet = wb.active\n\n# Add data\nsheet['A1'] = 'Hello'\nsheet['B1'] = 'World'\nsheet.append(['Row', 'of', 'data'])\n\n# Add formula\nsheet['B2'] = '=SUM(A1:A10)'\n\n# Formatting\nsheet['A1'].font = Font(bold=True, color='FF0000')\nsheet['A1'].fill = PatternFill('solid', start_color='FFFF00')\nsheet['A1'].alignment = Alignment(horizontal='center')\n\n# Column width\nsheet.column_dimensions['A'].width = 20\n\nwb.save('output.xlsx')\n```\n\n### Editing existing Excel files\n\n```python\n# Using openpyxl to preserve formulas and formatting\nfrom openpyxl import load_workbook\n\n# Load existing file\nwb = load_workbook('existing.xlsx')\nsheet = wb.active  # or wb['SheetName'] for specific sheet\n\n# Working with multiple sheets\nfor sheet_name in wb.sheetnames:\n    sheet = wb[sheet_name]\n    print(f\"Sheet: {sheet_name}\")\n\n# Modify cells\nsheet['A1'] = 'New Value'\nsheet.insert_rows(2)  # Insert row at position 2\nsheet.delete_cols(3)  # Delete column 3\n\n# Add new sheet\nnew_sheet = wb.create_sheet('NewSheet')\nnew_sheet['A1'] = 'Data'\n\nwb.save('modified.xlsx')\n```\n\n## Recalculating formulas\n\nExcel files created or modified by openpyxl contain formulas as strings but not calculated values. Use the provided `scripts\/recalc.py` script to recalculate formulas:\n\n```bash\npython scripts\/recalc.py <excel_file> [timeout_seconds]\n```\n\nExample:\n```bash\npython scripts\/recalc.py output.xlsx 30\n```\n\nThe script:\n- Automatically sets up LibreOffice macro on first run\n- Recalculates all formulas in all sheets\n- Scans ALL cells for Excel errors (#REF!, #DIV\/0!, etc.)\n- Returns JSON with detailed error locations and counts\n- Works on both Linux and macOS\n\n## Formula Verification Checklist\n\nQuick checks to ensure formulas work correctly:\n\n### Essential Verification\n- [ ] **Test 2-3 sample references**: Verify they pull correct values before building full model\n- [ ] **Column mapping**: Confirm Excel columns match (e.g., column 64 = BL, not BK)\n- [ ] **Row offset**: Remember Excel rows are 1-indexed (DataFrame row 5 = Excel row 6)\n\n### Common Pitfalls\n- [ ] **NaN handling**: Check for null values with `pd.notna()`\n- [ ] **Far-right columns**: FY data often in columns 50+ \n- [ ] **Multiple matches**: Search all occurrences, not just first\n- [ ] **Division by zero**: Check denominators before using `\/` in formulas (#DIV\/0!)\n- [ ] **Wrong references**: Verify all cell references point to intended cells (#REF!)\n- [ ] **Cross-sheet references**: Use correct format (Sheet1!A1) for linking sheets\n\n### Formula Testing Strategy\n- [ ] **Start small**: Test formulas on 2-3 cells before applying broadly\n- [ ] **Verify dependencies**: Check all cells referenced in formulas exist\n- [ ] **Test edge cases**: Include zero, negative, and very large values\n\n### Interpreting scripts\/recalc.py Output\nThe script returns JSON with error details:\n```json\n{\n  \"status\": \"success\",           \/\/ or \"errors_found\"\n  \"total_errors\": 0,              \/\/ Total error count\n  \"total_formulas\": 42,           \/\/ Number of formulas in file\n  \"error_summary\": {              \/\/ Only present if errors found\n    \"#REF!\": {\n      \"count\": 2,\n      \"locations\": [\"Sheet1!B5\", \"Sheet1!C10\"]\n    }\n  }\n}\n```\n\n## Best Practices\n\n### Library Selection\n- **pandas**: Best for data analysis, bulk operations, and simple data export\n- **openpyxl**: Best for complex formatting, formulas, and Excel-specific features\n\n### Working with openpyxl\n- Cell indices are 1-based (row=1, column=1 refers to cell A1)\n- Use `data_only=True` to read calculated values: `load_workbook('file.xlsx', data_only=True)`\n- **Warning**: If opened with `data_only=True` and saved, formulas are replaced with values and permanently lost\n- For large files: Use `read_only=True` for reading or `write_only=True` for writing\n- Formulas are preserved but not evaluated - use scripts\/recalc.py to update values\n\n### Working with pandas\n- Specify data types to avoid inference issues: `pd.read_excel('file.xlsx', dtype={'id': str})`\n- For large files, read specific columns: `pd.read_excel('file.xlsx', usecols=['A', 'C', 'E'])`\n- Handle dates properly: `pd.read_excel('file.xlsx', parse_dates=['date_column'])`\n\n## Code Style Guidelines\n**IMPORTANT**: When generating Python code for Excel operations:\n- Write minimal, concise Python code without unnecessary comments\n- Avoid verbose variable names and redundant operations\n- Avoid unnecessary print statements\n\n**For Excel files themselves**:\n- Add comments to cells with complex formulas or important assumptions\n- Document data sources for hardcoded values\n- Include notes for key calculations and model sections",
                "num_lines": 292,
                "start_line": 1,
                "total_lines": 292
            }
        },
        {
            "type": "text",
            "text": "Now I'll create the budget spreadsheet following the skill instructions."
        },
        {
            "type": "server_tool_use",
            "id": "srvtoolu_01Mn9rUtbkm9xNLEzzzRcVyu",
            "name": "text_editor_code_execution",
            "input": {
                "command": "create",
                "path": "\/tmp\/create_budget.py",
                "file_text": "from openpyxl import Workbook\nfrom openpyxl.styles import Font, PatternFill, Alignment, Border, Side, numbers\nfrom openpyxl.utils import get_column_letter\n\nwb = Workbook()\nws = wb.active\nws.title = \"Monthly Budget\"\n\n# Colors\nheader_fill = PatternFill(\"solid\", fgColor=\"2F5496\")\ncategory_fill = PatternFill(\"solid\", fgColor=\"D6E4F0\")\ntotal_fill = PatternFill(\"solid\", fgColor=\"B4C6E7\")\nincome_fill = PatternFill(\"solid\", fgColor=\"C6EFCE\")\nsurplus_fill = PatternFill(\"solid\", fgColor=\"FFF2CC\")\nheader_font = Font(name=\"Arial\", bold=True, color=\"FFFFFF\", size=12)\ntitle_font = Font(name=\"Arial\", bold=True, size=16, color=\"2F5496\")\ncategory_font = Font(name=\"Arial\", bold=True, size=11, color=\"2F5496\")\ntotal_font = Font(name=\"Arial\", bold=True, size=11)\ninput_font = Font(name=\"Arial\", size=11, color=\"0000FF\")  # Blue for hardcoded inputs\nformula_font = Font(name=\"Arial\", size=11, color=\"000000\")  # Black for formulas\nthin_border = Border(\n    left=Side(style=\"thin\"), right=Side(style=\"thin\"),\n    top=Side(style=\"thin\"), bottom=Side(style=\"thin\")\n)\nthick_bottom = Border(\n    left=Side(style=\"thin\"), right=Side(style=\"thin\"),\n    top=Side(style=\"thin\"), bottom=Side(style=\"medium\")\n)\n\n# Column widths\nws.column_dimensions[\"A\"].width = 28\nws.column_dimensions[\"B\"].width = 16\nws.column_dimensions[\"C\"].width = 16\nws.column_dimensions[\"D\"].width = 18\n\n# Title\nws.merge_cells(\"A1:D1\")\nws[\"A1\"] = \"Monthly Budget Planner\"\nws[\"A1\"].font = title_font\nws[\"A1\"].alignment = Alignment(horizontal=\"center\", vertical=\"center\")\nws.row_dimensions[1].height = 36\n\n# Headers\nheaders = [\"Category\", \"Budgeted ($)\", \"Actual ($)\", \"Difference ($)\"]\nfor col, h in enumerate(headers, 1):\n    cell = ws.cell(row=3, column=col, value=h)\n    cell.font = header_font\n    cell.fill = header_fill\n    cell.alignment = Alignment(horizontal=\"center\", vertical=\"center\")\n    cell.border = thin_border\nws.row_dimensions[3].height = 24\n\n# Data structure: (label, budgeted, actual) — None means it's a category header\ndata = [\n    (\"INCOME\", None, None),\n    (\"Salary\", 5000, 5000),\n    (\"Freelance \/ Side Income\", 500, 650),\n    (\"Other Income\", 100, 75),\n    (\"TOTAL INCOME\", \"SUM\", \"SUM\"),\n\n    (\"HOUSING\", None, None),\n    (\"Rent \/ Mortgage\", 1400, 1400),\n    (\"Utilities (Electric, Water, Gas)\", 200, 185),\n    (\"Internet \/ Phone\", 120, 120),\n    (\"TOTAL HOUSING\", \"SUM\", \"SUM\"),\n\n    (\"TRANSPORTATION\", None, None),\n    (\"Car Payment\", 350, 350),\n    (\"Gas \/ Fuel\", 150, 175),\n    (\"Insurance\", 100, 100),\n    (\"TOTAL TRANSPORTATION\", \"SUM\", \"SUM\"),\n\n    (\"FOOD\", None, None),\n    (\"Groceries\", 400, 430),\n    (\"Dining Out\", 150, 200),\n    (\"TOTAL FOOD\", \"SUM\", \"SUM\"),\n\n    (\"PERSONAL & LIFESTYLE\", None, None),\n    (\"Health \/ Gym\", 50, 50),\n    (\"Clothing\", 75, 40),\n    (\"Entertainment \/ Subscriptions\", 100, 110),\n    (\"Personal Care\", 50, 60),\n    (\"TOTAL PERSONAL & LIFESTYLE\", \"SUM\", \"SUM\"),\n\n    (\"SAVINGS & DEBT\", None, None),\n    (\"Emergency Fund\", 300, 300),\n    (\"Retirement (401k \/ IRA)\", 400, 400),\n    (\"Extra Debt Payment\", 100, 50),\n    (\"TOTAL SAVINGS & DEBT\", \"SUM\", \"SUM\"),\n]\n\ncurr_fmt = '$#,##0;($#,##0);\"-\"'\nrow = 4\ncategory_start = None\nincome_total_row = None\nexpense_total_rows = []\n\nfor label, budgeted, actual in data:\n    if budgeted is None:  # Category header\n        cell = ws.cell(row=row, column=1, value=label)\n        cell.font = category_font\n        cell.fill = category_fill\n        cell.border = thin_border\n        for c in range(2, 5):\n            ws.cell(row=row, column=c).fill = category_fill\n            ws.cell(row=row, column=c).border = thin_border\n        ws.merge_cells(start_row=row, start_column=1, end_row=row, end_column=4)\n        category_start = row + 1\n        row += 1\n        continue\n\n    ws.cell(row=row, column=1, value=label).border = thin_border\n\n    if budgeted == \"SUM\":  # Total row\n        sum_start = category_start\n        sum_end = row - 1\n        b_formula = f\"=SUM(B{sum_start}:B{sum_end})\"\n        c_formula = f\"=SUM(C{sum_start}:C{sum_end})\"\n        d_formula = f\"=C{row}-B{row}\"\n\n        for c in range(1, 5):\n            ws.cell(row=row, column=c).fill = total_fill\n            ws.cell(row=row, column=c).border = thick_bottom\n\n        ws.cell(row=row, column=1).font = total_font\n        cell_b = ws.cell(row=row, column=2, value=b_formula)\n        cell_b.font = total_font\n        cell_b.number_format = curr_fmt\n        cell_b.alignment = Alignment(horizontal=\"center\")\n\n        cell_c = ws.cell(row=row, column=3, value=c_formula)\n        cell_c.font = total_font\n        cell_c.number_format = curr_fmt\n        cell_c.alignment = Alignment(horizontal=\"center\")\n\n        cell_d = ws.cell(row=row, column=4, value=d_formula)\n        cell_d.font = total_font\n        cell_d.number_format = curr_fmt\n        cell_d.alignment = Alignment(horizontal=\"center\")\n\n        if \"INCOME\" in label:\n            income_total_row = row\n        else:\n            expense_total_rows.append(row)\n\n        row += 1\n        continue\n\n    # Regular data row\n    cell_b = ws.cell(row=row, column=2, value=budgeted)\n    cell_b.font = input_font\n    cell_b.number_format = curr_fmt\n    cell_b.alignment = Alignment(horizontal=\"center\")\n    cell_b.border = thin_border\n\n    cell_c = ws.cell(row=row, column=3, value=actual)\n    cell_c.font = input_font\n    cell_c.number_format = curr_fmt\n    cell_c.alignment = Alignment(horizontal=\"center\")\n    cell_c.border = thin_border\n\n    diff_formula = f\"=C{row}-B{row}\"\n    cell_d = ws.cell(row=row, column=4, value=diff_formula)\n    cell_d.font = formula_font\n    cell_d.number_format = curr_fmt\n    cell_d.alignment = Alignment(horizontal=\"center\")\n    cell_d.border = thin_border\n\n    ws.cell(row=row, column=1).font = Font(name=\"Arial\", size=11)\n    row += 1\n\n# Summary section\nrow += 1\nws.merge_cells(start_row=row, start_column=1, end_row=row, end_column=4)\ncell = ws.cell(row=row, column=1, value=\"BUDGET SUMMARY\")\ncell.font = Font(name=\"Arial\", bold=True, size=14, color=\"2F5496\")\ncell.alignment = Alignment(horizontal=\"center\")\nfor c in range(1, 5):\n    ws.cell(row=row, column=c).fill = PatternFill(\"solid\", fgColor=\"2F5496\")\nws.cell(row=row, column=1).font = Font(name=\"Arial\", bold=True, size=14, color=\"FFFFFF\")\nrow += 1\n\n# Total Income\nexpense_sum_b = \"+\".join([f\"B{r}\" for r in expense_total_rows])\nexpense_sum_c = \"+\".join([f\"C{r}\" for r in expense_total_rows])\n\nsummary_items = [\n    (\"Total Income\", f\"=B{income_total_row}\", f\"=C{income_total_row}\"),\n    (\"Total Expenses\", f\"={expense_sum_b}\", f\"={expense_sum_c}\"),\n    (\"Net Surplus \/ (Deficit)\", f\"=B{row}-B{row+1}\", f\"=C{row}-C{row+1}\"),\n]\n\nfor i, (label, b_val, c_val) in enumerate(summary_items):\n    r = row + i\n    ws.cell(row=r, column=1, value=label).font = total_font\n    ws.cell(row=r, column=1).border = thin_border\n\n    cell_b = ws.cell(row=r, column=2, value=b_val)\n    cell_b.font = total_font\n    cell_b.number_format = curr_fmt\n    cell_b.alignment = Alignment(horizontal=\"center\")\n    cell_b.border = thin_border\n\n    cell_c = ws.cell(row=r, column=3, value=c_val)\n    cell_c.font = total_font\n    cell_c.number_format = curr_fmt\n    cell_c.alignment = Alignment(horizontal=\"center\")\n    cell_c.border = thin_border\n\n    d_formula = f\"=C{r}-B{r}\"\n    cell_d = ws.cell(row=r, column=4, value=d_formula)\n    cell_d.font = total_font\n    cell_d.number_format = curr_fmt\n    cell_d.alignment = Alignment(horizontal=\"center\")\n    cell_d.border = thin_border\n\n    if label == \"Net Surplus \/ (Deficit)\":\n        for c in range(1, 5):\n            ws.cell(row=r, column=c).fill = surplus_fill\n            ws.cell(row=r, column=c).border = thick_bottom\n\n# Freeze panes\nws.freeze_panes = \"A4\"\n\n# Print settings\nws.sheet_properties.pageSetUpPr = None\nws.print_title_rows = \"1:3\"\n\noutput_path = \"\/tmp\/monthly_budget.xlsx\"\nwb.save(output_path)\nprint(f\"Saved to {output_path}\")\n"
            },
            "caller": {
                "type": "direct"
            }
        },
        {
            "type": "text_editor_code_execution_tool_result",
            "tool_use_id": "srvtoolu_01Mn9rUtbkm9xNLEzzzRcVyu",
            "content": {
                "type": "text_editor_code_execution_create_result",
                "is_file_update": false
            }
        },
        {
            "type": "server_tool_use",
            "id": "srvtoolu_01BNjqN8cQ4gkFNx5jmEH2nW",
            "name": "bash_code_execution",
            "input": {
                "command": "cd \/tmp && python create_budget.py"
            },
            "caller": {
                "type": "direct"
            }
        },
        {
            "type": "bash_code_execution_tool_result",
            "tool_use_id": "srvtoolu_01BNjqN8cQ4gkFNx5jmEH2nW",
            "content": {
                "type": "bash_code_execution_result",
                "stdout": "Saved to \/tmp\/monthly_budget.xlsx\n",
                "stderr": "",
                "return_code": 0,
                "content": [
                ]
            }
        },
        {
            "type": "text",
            "text": "Now let me recalculate the formulas using the provided script:"
        },
        {
            "type": "server_tool_use",
            "id": "srvtoolu_018RVPmPdQRbwB6ieHj9Lgfd",
            "name": "bash_code_execution",
            "input": {
                "command": "python \/skills\/xlsx\/scripts\/recalc.py \/tmp\/monthly_budget.xlsx"
            },
            "caller": {
                "type": "direct"
            }
        },
        {
            "type": "bash_code_execution_tool_result",
            "tool_use_id": "srvtoolu_018RVPmPdQRbwB6ieHj9Lgfd",
            "content": {
                "type": "bash_code_execution_result",
                "stdout": "{\n  \"status\": \"success\",\n  \"total_errors\": 0,\n  \"error_summary\": {},\n  \"total_formulas\": 45\n}\n",
                "stderr": "",
                "return_code": 0,
                "content": [
                ]
            }
        },
        {
            "type": "text",
            "text": "All 45 formulas verified with zero errors. Let me export the file:"
        },
        {
            "type": "server_tool_use",
            "id": "srvtoolu_01MEXreCF74VEHMmArD4AyQ6",
            "name": "bash_code_execution",
            "input": {
                "command": "cp \/tmp\/monthly_budget.xlsx \"$OUTPUT_DIR\/monthly_budget.xlsx\""
            },
            "caller": {
                "type": "direct"
            }
        },
        {
            "type": "bash_code_execution_tool_result",
            "tool_use_id": "srvtoolu_01MEXreCF74VEHMmArD4AyQ6",
            "content": {
                "type": "bash_code_execution_result",
                "stdout": "",
                "stderr": "",
                "return_code": 0,
                "content": [
                    {
                        "type": "bash_code_execution_output",
                        "file_id": "file_011CYFimkP5krGhRedLGQMxE"
                    }
                ]
            }
        },
        {
            "type": "text",
            "text": "Your **Monthly Budget Planner** is ready! Here's what's inside:\n\n### 📊 Structure\nThe spreadsheet has **6 budget categories** with Budgeted, Actual, and Difference columns:\n\n| Category | What's Included |\n|---|---|\n| **Income** | Salary, Freelance\/Side Income, Other Income |\n| **Housing** | Rent\/Mortgage, Utilities, Internet\/Phone |\n| **Transportation** | Car Payment, Gas\/Fuel, Insurance |\n| **Food** | Groceries, Dining Out |\n| **Personal & Lifestyle** | Health\/Gym, Clothing, Entertainment, Personal Care |\n| **Savings & Debt** | Emergency Fund, Retirement, Extra Debt Payment |\n\n### ✨ Features\n- **Fully formula-driven** — 45 live formulas that auto-update when you change any values\n- **Difference column** automatically calculates over\/under for every line item\n- **Budget Summary** at the bottom shows Total Income, Total Expenses, and Net Surplus\/(Deficit)\n- **Color-coded**: Blue text = editable inputs, black text = formulas, category headers and totals are shaded for easy scanning\n- **Frozen header row** so labels stay visible as you scroll\n\nSimply update the blue numbers in the **Budgeted** and **Actual** columns with your own figures, and everything recalculates automatically!"
        }
    ],
    "container": {
        "id": "container_011CYFihQjn85aatqfv5srYU",
        "expires_at": "2026-02-18T15:50:18.065207Z",
        "skills": [
            {
                "type": "anthropic",
                "skill_id": "xlsx",
                "version": "20260203"
            }
        ]
    },
    "stop_reason": "end_turn",
    "stop_sequence": null,
    "usage": {
        "input_tokens": 45795,
        "cache_creation_input_tokens": 0,
        "cache_read_input_tokens": 0,
        "cache_creation": {
            "ephemeral_5m_input_tokens": 0,
            "ephemeral_1h_input_tokens": 0
        },
        "output_tokens": 3629,
        "service_tier": "standard",
        "inference_geo": "global",
        "server_tool_use": {
            "web_search_requests": 0,
            "web_fetch_requests": 0
        }
    }

```

<br>

### How to Use the Model Outputs

In addition to returning a detailed explanation of the methodology it applied, the model also generated an Excel file named `budget.xlsx`.

This file is available for download. Its identifier can be retrieved via the Files API using the [List files](https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/files-api.md#1-list-of-files) endpoint.

```text
file_011CYFimkP5krGhRedLGQMxE
    • filename: monthly_budget.xlsx
    • mimeType: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
    • sizeBytes: 7510
    • downloadable: True
``` 

You can download the generated file at any time using the Files API with the [Download file](https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/files-api.md#4-download-a-file) endpoint.

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

- [Using Skills with the API](https://platform.claude.com/docs/en/build-with-claude/skills-guide)
- [Agent Skills overview](https://platform.claude.com/docs/en/agents-and-tools/agent-skills/overview)
- [Best practices](https://platform.claude.com/docs/en/agents-and-tools/agent-skills/best-practices) for authoring Skills
- [Code execution tool documentation](https://platform.claude.com/docs/en/agents-and-tools/agent-skills/enterprise)
- Custom Skill lifecycle and API details: see [agent-skills-custom](agent-skills-custom.md)
