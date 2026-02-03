# Document understanding
Multimodal data may be supplied inline as `base64-encoded` content or via the Files API for larger files.

- [Image understanding](#image-understanding)
- [Document (PDF) understanding](#document-pdf-understanding)
___

>[!NOTE]
> The code snippets will exclusively refer to the `procedure (Params: TChatParams)` (the message payload), as introduced in the sections covering [non-streamed](content-generation.md#content-generation-non-streamed) and [streamed](content-generation-sse.md#content-generation-sse-streaming) generation.

<br>

## Image understanding

The wrapper provides full support for handling base64-encoded content as well as DATA URI formats. For more details, refer to the [`Anthropic.Net.MediaCodec`](https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/source/Anthropic.Net.MediaCodec.pas) unit, which includes the `TMediaCodec` helper, or to the Codec management section.

Anthropic Claude currently supports JPEG, PNG, GIF, and WebP image formats, specifically:
- **image/jpeg**
- **image/png**
- **image/gif**
- **image/webp**

<br>

### Base64-encoded image

```pascal
  //uses Anthropic, Anthropic.Types, Anthropic.Helpers; 

  var ImageLocation := '..\..\media\Invoice.png';
  var Base64 := TMediaCodec.EncodeBase64(ImageLocation);
  var MimeType := TMediaCodec.GetMimeType(ImageLocation);
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var Prompt := 'Describe the image.';

  //JSON payload generation
  var Params: TProc<TChatParams> :=
        procedure (Params: TChatParams)
        begin
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Messages( Generation.MessageParts
              .User( Generation.ContentParts
                  .AddImage(base64, mimeType)
                  .AddText(Prompt)
              )
            )
            //.Stream;  //Optional: only when streaming mode
        end;
```

- The `Generation` helper record provides a convenient way to add various types of inputs, including images and documents. It is also possible to supply only a document URI, without embedding the content itself. 

<br>

### URL-based image

```pascal
  //uses Anthropic, Anthropic.Types, Anthropic.Helpers; 

  var ImageUrl := 'https://assets.visitorscoverage.com/production/wp-content/uploads/2024/04/AdobeStock_626542468-min-1024x683.jpeg'; 

  var ImageLocation := '..\..\media\Invoice.png';
  var Base64 := TMediaCodec.EncodeBase64(ImageLocation);
  var MimeType := TMediaCodec.GetMimeType(ImageLocation);

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var Prompt := 'Compare images';

  //JSON payload generation
  var Payload: TProc<TChatParams> :=
  procedure (Params: TChatParams)
  begin
    with Generation do
      Params
        .Model(ModelName)
        .MaxTokens(MaxTokens)
        .Messages( MessageParts
            .User( ContentParts
                .AddImage(base64, mimeType)
                .AddImage(ImageUrl)
                .AddText(Prompt)
            )
        )
        //.Stream;  //Optional: only when streaming mode
  end;   
```

- The `Generation` helper record is available in the `Anthropic.Helpers.pas` unit. This unit contains several helper classes designed to simplify the construction of JSON structures.

<br>

### Files API image

To avoid repeatedly re-encoding recurring images, use the Files API.

```pascal
  var FileId := 'file_123abc';
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var Prompt := 'prompt value';

  //JSON payload generation
  var Payload: TProc<TChatParams> :=
  procedure (Params: TChatParams)
  begin
    with Generation do
      Params
        .Model(ModelName)
        .MaxTokens(MaxTokens)
        .Messages( MessageParts
            .User( ContentParts
                .AddImage(FileId)
                .AddText(Prompt)
            )
        )
  end; 
```

<br>

### Delphi `version 12 or later`

With Delphi versions 12 and later, it is possible to use multiline strings, which makes it possible to:
 
```pascal
  //uses Anthropic, Anthropic.Types, Anthropic.Helpers; 

  var ImageLocation := '..\..\media\Invoice.png';
  var Base64 := TMediaCodec.EncodeBase64(ImageLocation);

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var Prompt := 'Describe the image.';

  //JSON payload generation
  var Payload: TProc<TChatParams> :=
  procedure (Params: TChatParams)
  begin
    with Generation do
      Params
        .Model(ModelName)
        .MaxTokens(MaxTokens)
        .Messages(
            Format(
              '''
              [
                {
                  "role": "user",
                  "content": [
                    {
                      "type": "image",
                      "source": {
                        "type": "base64",
                        "media_type": "image/jpeg",
                        "data": "%s"
                      }
                    },
                    {
                      "type": "text",
                      "text": "%s"
                    }
                  ]
                }
              ]
              ''',
              [Base64, Prompt])
        )
        //.Stream;  //Optional: only when streaming mode
  end;
```

<br>

>[!IMPORTANT]
>- If you submit an image larger than 8000x8000 px, it will be rejected.
>- While the API supports 100 images per request, there is a 32MB request size limit for standard endpoints.
>- PDF and image inputs rely on Claude’s vision capabilities and therefore share the same general limitations as image understanding tasks.

See [official documentation](https://platform.claude.com/docs/en/build-with-claude/vision).

<br>

## Document (PDF) understanding

Document Processing with Anthropic Models

`Anthropic models` can process documents in `PDF` format using native vision capabilities to understand the document as a whole, rather than relying solely on text extraction. This enables advanced document-level analysis, including:
- Analyzing and interpreting content that combines text, images, diagrams, charts, and tables, even in long documents of up to **100 pages**.
- Extracting information into structured output formats.
- Generating summaries and answering questions based on both visual and textual elements.
- Transcribing document content (for example, to HTML) while preserving layout and formatting for downstream use.

Non-PDF documents can also be provided using the same mechanism; however, in this case, Anthropic processes them as plain text, which means visual context such as charts, layout, and formatting is not preserved.

<br>

### Passing PDF data inline

```pascal
   //uses Anthropic, Anthropic.Types, Anthropic.Helpers; 

  var FilePath := '..\..\media\File_Search_file.pdf';
  var Base64 := TMediaCodec.EncodeBase64(FilePath);
  var MimeType := 'application/pdf';

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1000;
  var Prompt := 'What is this document about?';

  //JSON payload generation
  var Payload: TProc<TChatParams> :=
        procedure (Params: TChatParams)
        begin
          with Generation do
            Params
              .Model(ModelName)
              .MaxTokens(MaxTokens)
              .Messages( MessageParts
                  .User( ContentParts
                      .AddPDF(Base64)
                      .AddText(Prompt)
                  )
              )
              //.Stream;  //Optional: only when streaming mode
        end;
```

<br>

### Passing PDF by API Files

When PDF files come from a local system or when no URL is available, they must be sent directly.

```pascal
  var FileId := 'file_456efg';
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var Prompt := 'Prompt value';

  //JSON payload generation
  var Payload: TProc<TChatParams> :=
        procedure (Params: TChatParams)
        begin
          with Generation do
            Params
              .Model(ModelName)
              .MaxTokens(MaxTokens)
              .Messages( MessageParts
                  .User( ContentParts
                      .AddPDF(FileId)
                      .AddText(Prompt)
                  )
              )
              //.Stream;  //Optional: only when streaming mode
        end;
```

<br>

### Passing PDF by URL

When PDF files are reused or when you want to eliminate repeated encoding costs, it’s best to upload them through the Files API.

```pascal
  var PdfUrl := 'https://assets.anthropic.com/m/1cd9d098ac3e6467/original/Claude-3-Model-Card-October-Addendum.pdf';
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var Prompt := 'Prompt value';

  //JSON payload generation
  var Payload: TProc<TChatParams> :=
        procedure (Params: TChatParams)
        begin
          with Generation do
            Params
              .Model(ModelName)
              .MaxTokens(MaxTokens)
              .Messages( MessageParts
                  .User( ContentParts
                      .AddPDF(PdfUrl)
                      .AddText(Prompt)
                  )
              )
              //.Stream;  //Optional: only when streaming mode
        end;
```

<br>

### Delphi `version 12 or later`

With Delphi versions 12 and later, it is possible to use multiline strings, which makes it possible to:


```pascal
  //uses Anthropic, Anthropic.Types, Anthropic.Helpers; 

  var PDFUrl := 'https://assets.anthropic.com/m/1cd9d098ac3e6467/original/Claude-3-Model-Card-October-Addendum.pdf';
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1000;
  var Prompt := 'What is this document about?';


  //JSON payload generation
  var Payload: TProc<TChatParams> :=
  procedure (Params: TChatParams)
  begin
    with Generation do
      Params
        .Model(ModelName)
        .MaxTokens(MaxTokens)
        .Messages(
            Format(
              '''
              [
                {
                   "role": "user",
                   "content": [{
                       "type": "document",
                       "source": {
                           "type": "url",
                           "url": "%s"
                       }
                   },
                   {
                       "type": "text",
                       "text": "%s"
                   }]
                }
              ]
              ''',
              [PDFUrl, Prompt])
        )
        //.Stream;  //Optional: only when streaming mode
  end;
```

### PDF Size and Processing Limits
Anthropic supports PDF documents up to **32 MB** in size or a maximum of **100 pages**. These limits apply to both inline submissions and uploads performed through the Files API. Each document page is internally accounted for as **258 tokens**.

See [official documentation](https://platform.claude.com/docs/fr/build-with-claude/pdf-support).


