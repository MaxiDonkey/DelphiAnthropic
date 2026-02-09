# Files API

The ***Files API*** provides a persistence and referencing layer for files, allowing uploads to be decoupled from their use in `Messages` calls, while offering standard file-management operations.

- [Uploading a file](#uploading-a-file)
- [Using a file in messages](#using-a-file-in-messages)
- [Working with other file formats](#working-with-other-file-formats)
- [Files API CRUD](#files-api-crud)
- [Quick Selection Guide](#quick-selection-guide)
- [Practical Notes](#practical-notes)

___

## Uploading a file

Uploading consists of storing a file once in the API’s secure storage in order to obtain a stable `file_id`.
This `file_id` can then be reused across multiple requests, avoiding repeated uploads and reducing bandwidth, latency, and token overhead.
Key constraints include a 500 MB maximum file size, 100 GB total storage per organization, and the requirement to enable the beta feature header.

<br>

```pascal
  // uses Anthropic, Anthropic.Types, Anthropic.Helpers, Anthropic.Async.Promise; 
  // Client: IAnthropic;

  var FilePath := '..\..\..\media\File_Search_file.pdf';

  // MultipartFormData payload creation
  var Payload: TUploadParamProc :=
    procedure (Params: TUploadParams)
    begin
      Params.&File(FilePath);
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Files.AsyncAwaitUpload(Payload);

  Promise
    .&Then(
      procedure (Value: TFile)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Files.Upload(Payload);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end
```

<br>

## Using a file in messages

After upload, a file is injected into a `Messages` request via a content block (`document` or `image`) that references the `file_id`.
The model then consumes the file as native context (e.g., PDF summarization, image analysis), with optional fields such as `title`, `context`, or `citations`.
Only the tokens actually read by the model are billed.

- JSON Payload creation - extended reasoning 

  ```pascal
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 1024;
    var Prompt := 'Please summarize this document for me.';

    var FileID := 'file_123abc';  //e.g. file_011CXwVCzfpsB998gitfsYNF

    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Beta(['files-api-2025-04-14'])
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Messages( MessageParts
                .User( ContentParts
                    .AddText(Prompt)
                    .AddFileDocument(FileId)
                )
            );
      end;
  ```

<br> 

## Working with other file formats

File formats that are not directly supported as `document` blocks (CSV, XLSX, DOCX, MD, etc.) must be preprocessed client-side.
The recommended approach is to convert them to plain text (or to PDF when appropriate) and include the resulting text directly in the message.
This makes the boundary explicit: the Files API handles storage and referencing, not semantic format conversion.

>[!NOTE]
>For .docx files containing images, convert them to PDF format first, then use PDF support to take advantage of the built-in image parsing. This allows using citations from the PDF document.

<br>

## Files API CRUD

The ***Files API*** exposes a standard lifecycle:
These operations are free of charge and independent from the token-based billing of `Messages` requests.

- [1. List of files](#1-list-of-files)
- [2. Retrieve a file by it's ID](#2-retrieve-a-file-by-its-id)
- [3. Delete a file by it's ID](#3-delete-a-file-by-its-id)
- [4. Download a file](#4-download-a-file)

___

### 1. List of files

```pascal
  var QueryParams: TFilesListParamProc :=
    procedure (Params: TFilesListParams)
    begin
      Params.Limit(5);
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Files.AsyncAwaitList(QueryParams);

  Promise
    .&Then(
      procedure (Value: TFileList)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Files.List(QueryParams);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

>[!NOTE]
> The `list` method may also be invoked with no parameters provided.

<br>

### 2. Retrieve a file by it's ID

```pascal
  var FileID := 'file_123abc'; //e.g. file_011CXwVCzfpsB998gitfsYNF

  // Asynchronous generation (promise-based)
  var Promise := Client.Files.AsyncAwaitRetrieve(FileID);

  Promise
    .&Then(
      procedure (Value: TFile)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Files.Retrieve(FileId);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

### 3. Delete a file by it's ID

```pascal
  var FileID := 'file_123abc'; //e.g. file_011CXwVCzfpsB998gitfsYNF

  // Asynchronous generation (promise-based)
  var Promise := Client.Files.AsyncAwaitDelete(FileId);

  Promise
    .&Then(
      procedure (Value: TFileDeleted)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Files.Delete(FileId);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

### 4. Download a file

Downloading is strictly limited to files generated by a model using skills, including PPTX (PowerPoint) and DOCX (Word) files.
Downloadable files are identified by the `downloadable` property on the `TFile` instance.

>[!WARNING]
>Files uploaded by the user are not eligible for download.


#### Example

```bash
file_011CXj8BCsVbNUVaJwoKxQHo
    • filename: budget.xlsx
    • mimeType: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
    • sizeBytes: 6774
    • downloadable: True  
```

The file above was generated using the `XLSX` skill and therefore corresponds to an Excel file.

<br>

#### Download the Excel file

```pascal
  var FileName := 'budget.xlsx';
  var FileID := 'file_011CXj8BCsVbNUVaJwoKxQHo';
  
  // Asynchronous generation (promise-based)
  var Promise := Client.Files.AsyncAwaitDownload(FileId);

  Promise
    .&Then(
      procedure (Value: TFileDownloaded)
      begin
        Value.SaveToFile(FileName);
        Display(TutorialHub, FileName + ' downloaded');
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Files.Download(FileId);
//
//  try
//    Value.SaveToFile(FileName);
//    Display(TutorialHub, FileName + ' downloaded');
//  finally
//    Value.Free;
//  end;
```


<br>

## Quick Selection Guide

- **Use the Files API** when a file is large, reused across multiple requests, or shared between tools and Messages.
- **Upload once, reference many times** if the same document or dataset is involved in iterative analysis or multi-step workflows.
- **Inline text instead of files** for small, one-off inputs where conversion overhead outweighs persistence benefits.
- **Prefer PDF** when working with rich documents (layout, images, citations) to leverage native parsing and referencing.
- **Use tool-generated files** when downstream download or inspection of outputs (charts, transformed data) is required.

<br>

## Practical Notes

- Always enable the correct beta header (files-api-2025-04-14) when interacting with the Files API.
- Monitor storage quotas (500 MB per file, 100 GB per organization) to avoid hard failures in production pipelines.
- Remember that uploaded files are not downloadable; only files generated by tools or skills can be retrieved.
- Deleting a file is irreversible and may affect in-flight or cached workflows that still reference its file_id.
- Token billing applies when file content is consumed by the model, not when files are stored or managed.
- Treat format conversion as a client-side responsibility for unsupported document types.

>[!IMPORTANT]
> SDK wrappers automatically add the `files-api-2025-04-14` beta header for all Files API operations.