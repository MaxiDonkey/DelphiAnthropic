# Citations

The citations feature establishes an explicit link between each model-generated claim and precise passages in the source documents, ensuring traceability, verifiability, and response robustness.

- [How to use citations](#how-to-use-citations)
- [Prompt caching with citations](#prompt-caching-with-citations)
- [Response structure with citations](#response-structure-with-citations)
- [Quick Selection Guide](#quick-selection-guide)
- [Practical Notes](#practical-notes)
- [Note on Haiku models and citations](#note-on-haiku-models-and-citations)
___

<br>

## How to use citations

Using citations relies on three core steps:
- **Provide citable documents:** plain text, PDF, or custom content, either embedded directly in the message or referenced via `file_id`.
- **Explicitly enable citations** by setting `citations.enabled=true` on all documents in the request (partial activation is not supported).
- **Query the model normally:** once enabled, Claude automatically inserts citations into its response, pointing to exact locations (character indices, PDF pages, or content blocks).

The selected document type determines the ***granularity*** of the citations: sentence-level or page-based (plain text/PDF) or fully controlled blocks (custom content).

### Using citations with message API (text/plain document)

- JSON Payload creation - using citations

  ```pascal
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 1024;
    var Prompt := 'What color is the grass and sky?';

    var DataTextPlain := 'The grass is green. The sky is blue.';
    var DocumentContext := 'This is a trustworthy document.';
    var DocumentTitle := 'My Document';

    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Messages( MessageParts
                .User( ContentParts
                    .AddTextPlain( CreateDocumentBlock
                         .Source( Document.PlainText
                             .Data( DataTextPlain )
                         )
                         .Title( DocumentTitle )
                         .Context( DocumentContext )
                         .Citations(True)
                    )
                    .AddText( Prompt )
                )
            );
      end;
  ```

- JSON Payload generated

  ```json
  {
      "model": "claude-opus-4-6",
      "max_tokens": 1024,
      "messages": [
          {
              "role": "user",
              "content": [
                  {
                      "type": "document",
                      "source": {
                          "type": "text",
                          "media_type": "text\/plain",
                          "data": "The grass is green. The sky is blue."
                      },
                      "title": "My Document",
                      "context": "This is a trustworthy document.",
                      "citations": {
                          "enabled": true
                      }
                  },
                  {
                      "type": "text",
                      "text": "What color is the grass and sky?"
                  }
              ]
          }
      ]
  }
  ```

<br>

### Using citations with message API (PDF document)

- JSON Payload creation - using citations 

  ```pascal
    var Document := '..\media\File_Search_file.pdf';
    var Base64 := TMediaCodec.EncodeBase64(Document);

    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 2048;
    var Prompt := 'How can the work documented in the PDF be relevant?';

    var DocumentContext := 'Context about the document that will not be cited from';
    var DocumentTitle := 'Document Title';

    //JSON payload generation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Messages( MessageParts
                .User( ContentParts
                    .AddPDF( CreateDocumentBlock
                         .Source( Document.Base64PDF
                             .Data( Base64 )
                         )
                         .Title( DocumentTitle )
                         .Context( DocumentContext )
                         .Citations(True)
                    )
                    .AddText( Prompt )
                )
            );
      end;
  ```

<br>

## Prompt caching with citations

Citations are compatible with prompt caching, with a clear separation of responsibilities:
- **Source documents** can be cached using `cache_control` (e.g., `ephemeral`) to optimize performance for long, reusable content.
- **Generated citation blocks** themselves are not cached, but they reference documents that may be.
- The cited text (`cited_text`) is **excluded from billable input tokens counting**, both in output and when reused in subsequent turns, making the mechanism cost-efficient.

The result is traceable responses with minimal token overhead, even in iterative RAG workflows.

### Using caching & citations with message API

- JSON Payload creation - using caching & citations

  ```pascal
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 1024;
    var Prompt := 'What does this document say about API features?';

    var DataTextPlain := 'This is a very long document with thousands of words...';

    StarPayload(Prompt);

    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Messages( MessageParts
                .User( ContentParts
                    .AddTextPlain( CreateDocumentBlock
                         .Source( Document.PlainText
                             .Data( DataTextPlain )
                         )
                         .Citations(True)
                         .CacheControl( Cache
                             .AddCacheControl  //Document caching '5m' by default
                         )
                    )
                    .AddText( Prompt )
                )
            );
      end;	
  ```

<br>

## Response structure with citations

When citations are enabled, the response is no longer a monolithic text but a **sequence of content blocks**:
- Each `text` block may carry one or more associated citations.
- Each citation precisely references:
  - the source document (`document_index`, `document_title`),
  - the exact location (character indices, page ranges, or content blocks),
  - the cited text itself (`cited_text`, provided for inspection).
- In streaming mode, citations are delivered via `citations_delta` events and dynamically attached to the current text block.

This structure allows generative reasoning and documentary evidence to be interleaved, which explains the incompatibility with strictly structured outputs (e.g., JSON schemas) because citations introduce dynamic content blocks that cannot be statically constrained by a schema.

- Message response using citation

  ```json
  {
      "model": "claude-opus-4-6",
      "id": "msg_01UuYAyW8Kue2Z5NfaoBgxpW",
      "type": "message",
      "role": "assistant",
      "content": [
          {
              "type": "text",
              "text": "According to the document:\n\n- "
          },
          {
              "citations": [
                  {
                      "type": "char_location",
                      "cited_text": "The grass is green. ",
                      "document_index": 0,
                      "document_title": "My Document",
                      "start_char_index": 0,
                      "end_char_index": 20
                  }
              ],
              "type": "text",
              "text": "The grass is green."
          },
          {
              "type": "text",
              "text": "\n- "
          },
          {
              "citations": [
                  {
                      "type": "char_location",
                      "cited_text": "The sky is blue.",
                      "document_index": 0,
                      "document_title": "My Document",
                      "start_char_index": 20,
                      "end_char_index": 36
                  }
              ],
              "type": "text",
              "text": "The sky is blue."
          }
      ],
      "stop_reason": "end_turn",
      "stop_sequence": null,
      "usage": {
          "input_tokens": 612,
          "cache_creation_input_tokens": 0,
          "cache_read_input_tokens": 0,
          "cache_creation": {
              "ephemeral_5m_input_tokens": 0,
              "ephemeral_1h_input_tokens": 0
          },
          "output_tokens": 54,
          "service_tier": "standard",
          "inference_geo": "global"
      }
  }
  ```

<br>

## Quick Selection Guide

Use the document type and configuration that best matches your citation needs:
- **Plain text documents**
  Best for narrative or prose content where sentence-level citations are sufficient.
  → Automatic sentence fragmentation, citations by character indices.

- **PDF documents**
  Best for formal documents (reports, papers, specs) with reliable text extraction.
  → Automatic sentence fragmentation, citations by page ranges.

- **Custom content documents**
  Best for structured data (lists, transcripts, RAG chunks) requiring precise control.
  → No additional fragmentation, citations by content block indices.

#### Rule of thumb:
If you care about where something appears (pages), use PDF.
If you care about exact fragments, use custom content.
If you want simplicity, use plain text.

<br>

## Practical Notes

- Citations must be ***enabled*** consistently: all documents in a request must have `citations.enabled=true`, or none at all.
- Citations apply ***only to document source content***; `title` and `context` are not citable.
- Image-based PDFs without extractable text cannot be cited.
- Citations are ***incompatible with structured outputs*** (strict JSON schemas).
- Use custom content documents when you need stable citation boundaries across model versions or prompt changes.
- In streaming scenarios, handle `citations_delta` events explicitly to avoid losing references.

<br>

## Note on Haiku models and citations

The Haiku family has a specific limitation regarding citations:
- **Haiku 3 does not support citations.**
  This means it cannot generate responses with document-linked citations, even if documents are provided and `citations.enabled=true` is set.
- All other **active Claude models** support citations, but Haiku should be excluded from any workflow that requires:
  - traceability to source documents,
  - verifiable RAG outputs,
  - compliance or audit-oriented responses.

**Practical implication:**
Haiku models are suitable for fast, low-latency, or cost-sensitive tasks where citations are not required, but they should not be used in pipelines that depend on grounded or inspectable answers.
