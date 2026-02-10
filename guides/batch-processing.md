# Batch processing

Batch processing means grouping many requests together to run asynchronously and at high throughput, instead of handling them one by one with immediate responses (this is the common thread across the four points below).

- [How the Message Batches API works](#how-the-message-batches-api-works)
- [Using the API](#Using-the-api)
- [Other operations](#other-operations)
- [Common issues and batch privacy](#common-issues-and-batch-privacy)
- [Quick Selection Guide](#quick-selection-guide)
- [Practical Notes](#practical-notes)

___

## How the Message Batches API works

The Message Batches API lets you submit ***one batch*** containing a list of Messages API requests, then have the server execute them ***asynchronously*** and ***independently***.
- **One batch = a collection** of requests (up to 100,000 requests or 256 MB, whichever comes first).
- Each request is identified by a `custom_id` and includes a `params` object that is the standard ***Messages API payload***.
- The batch follows a simple lifecycle:
  - created → `processing_status: in_progress`
  - finished → `processing_status: ended`
- Requests can be ***mixed*** within a single batch (vision, tool use, system prompts, multi-turn, beta features), since each entry is processed separately.
- A batch can ***expire*** if some requests can’t be sent within 24 hours; results are then available for ***29 days***.

<br>




## Using the API

### Prepare and create the batch

Build a `requests` list where each item contains:
- `custom_id`: a unique identifier (your join key when reading results)
- `params`: normal Messages API parameters (`model`, `max_tokens`, `messages`, optionally `system`, etc.)

At creation time, validation of `params` is ***asynchronous***: schema errors may only surface when the batch ends. That’s why it’s recommended to ***test a single request*** with the Messages API first.

<br>

#### Example

As a first step, all requests in the batch are consolidated into a text file (`BatchExample.jsonl`), for example.

```json
{ "custom_id": "my-first-request","params":{"model":"claude-sonnet-4-5", "max_tokens":  1024,"messages":[{"role":"user","content":"Hello, world"}]}}
{"custom_id":"my-second-request",  "params":{"model":"claude-sonnet-4-5","max_tokens":1024,"messages":[{"role":"user","content":"Hi again, friend"}]}}
```

As a second step, the batch is submitted in order to retrieve the identifier of the associated batch processing job.

```pascal
  var Jsonl := TJSONLHelper.LoadFromFile('..\media\BatchExample.jsonl');

  //JSON payload
  var Payload: TRequestParamProc :=
     procedure (Params: TRequestParams)
     begin
       Params
         .Requests(JsonL);
     end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Batch.AsyncAwaitCreate(Payload);

  Promise
    .&Then(
      procedure (Value: TBatch)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Batch.Create(Payload);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

Return values

```bash
msgbatch_01LAMivETDrH9q5hno9RAQRs  <--- Job ID
    • Type: message_batch
    • Processing_status: in_progress
    • CreatedAt: 2026-02-10T14:14:56.498078+00:00
    • ExpiresAt: 2026-02-11T14:14:56.498078+00:00
    • Processing: 2
    ...
```

<br>

### Track the batch (status / polling) 

Typical options:
- Use the **Console** (workspace batches)
- **Poll** the "retrieve" endpoint until `processing_status == "ended"` (usually with a periodic sleep).

You can also list all batches in a workspace (with pagination) to find batch IDs.

<br>

### Retrieve results

Once the batch is `ended`, a `results_url` lets you stream the results, typically as **JSONL** (one JSON object per line).

Each request ends in one of four states:
- `succeeded`: success (message returned)
- `errored`: failed (e.g., invalid_request, internal). Not billed.
- `canceled`: not sent because the batch was canceled first. Not billed.
- `expired`: not sent before the 24-hour batch deadline. Not billed.

Key point: output order is not guaranteed—always match using `custom_id`.

<br>

Using the API, the process status is monitored via the `Retrieve` or `List` operations.

#### Retrieve API

```pascal
  var BatchID := 'msgbatch_01LAMivETDrH9q5hno9RAQRs';

  // Asynchronous example
  var Promise := Client.Batch.AsyncAwaitRetrieve(BatchID);

  Promise
    .&Then(
      procedure (Value: TBatch)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Batch := Client.Batch.Retrieve(BatchID);
//
//  try
//    Display(TutorialHub, Batch);
//  finally
//    Batch.Free;
//  end;
```

Return values

```bash
msgbatch_01LAMivETDrH9q5hno9RAQRs
    • Type: message_batch
    • Processing_status: ended
    • CreatedAt: 2026-02-10T14:14:56.498078+00:00
    • ExpiresAt: 2026-02-11T14:14:56.498078+00:00
    • ResultsUrl: https://api.anthropic.com/v1/messages/batches/msgbatch_01LAMivETDrH9q5hno9RAQRs/results
    • Processing: 0
    • Succeeded: 2
    • Errored: 0
    • Canceled: 0
    • Expired: 0
```

>[!NOTE]
> Here, we can see that the batch processing has completed. In addition, a URL is provided to retrieve the resulting output.

<br>

#### List API

This API returns all jobs, whether they are running, completed, or cancelled.

```pascal
  //Query params creation
  var QueryParams: TListParamProc :=
    procedure (Params: TListParams)
    begin
      Params.Limit(20);
    end;

  // Asynchronous example
  var Promise := Client.Batch.AsyncAwaitList(QueryParams);

  Promise
    .&Then(
      procedure (Value: TBatchList)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Batch := Client.Batch.List(QueryParams);
//
//  try
//    Display(TutorialHub, Batch);
//  finally
//    Batch.Free;
//  end;
```

<br>




## Other operations

### Cancel a batch

You can cancel a batch that’s still running:
- status becomes `canceling`
- then moves to `ended`
- you may get partial results for requests that completed before cancellation took effect.

<br>

```pascal
  var BatchID := 'msgbatch_123abc';  // The batch status must be set to in_progress 

  // Asynchronous example
  var Promise := Client.Batch.AsyncAwaitCancel(BatchID);

  Promise
    .&Then(
      procedure (Value: TBatch)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Batch := Client.Batch.Cancel(BatchId);
//  try
//    Display(TutorialHub, Batch);
//  finally
//    Batch.Free;
//  end;
```

<br>

### Prompt caching with batches

Batching supports prompt caching: you can mark large shared context blocks using `cache_control` (e.g., `ephemeral`) to improve cache hit rates.

Best practices to maximize cache hits:
1. Use **identical** `cache_control` blocks in every request in the batch
2. Keep a steady request stream to reduce cache expiration
3. Factor as much shared context as possible (same system prompt + same large document)

Benefit: additional cost/time savings on top of batching discounts, though cache hits are “best effort” because requests are processed concurrently and out of order.

<br>

### Récupération du lot traité

At this stage, it is sufficient to provide the batch identifier along with the name of a file (in JSONL format) intended to receive the processed values via the `Retrieve` API, as shown in the example below.
Internally, as described earlier, once a batch has been processed, a URL for retrieving the results is made available for 29 days; this mechanism is what is being leveraged here.

```pascal
  StartRun('Download batch Results');

  var BatchID := 'msgbatch_01LAMivETDrH9q5hno9RAQRs';
  var BatchResultFileName := 'BatchResult.jsonl';

  // Asynchronous example
  var Promise := Client.Batch.AsyncAwaitRetrieve(BatchID, BatchResultFileName);

  Promise
    .&Then(
      procedure (Value: TStringList)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Batch := Client.Batch.Retrieve(BatchID, BatchResultFileName);
//
//  try
//    Display(TutorialHub, Batch);
//  finally
//    Batch.Free;
//  end;
```

Return values

```json
{"custom_id":"my-first-request","result":{"type":"succeeded","message":{"model":"claude-sonnet-4-5-20250929","id":"msg_017SY5ievvNM8fnNvtSeLZN6","type":"message","role":"assistant","content":[{"type":"text","text":"Hello! How can I help you today?"}],"stop_reason":"end_turn","stop_sequence":null,"usage":{"input_tokens":10,"cache_creation_input_tokens":0,"cache_read_input_tokens":0,"cache_creation":{"ephemeral_5m_input_tokens":0,"ephemeral_1h_input_tokens":0},"output_tokens":12,"service_tier":"batch","inference_geo":"not_available"}}}}
{"custom_id":"my-second-request","result":{"type":"succeeded","message":{"model":"claude-sonnet-4-5-20250929","id":"msg_01B3RHE29BGLhuEJbWsrKDPr","type":"message","role":"assistant","content":[{"type":"text","text":"Hello again! Good to see you back. How can I help you today?"}],"stop_reason":"end_turn","stop_sequence":null,"usage":{"input_tokens":11,"cache_creation_input_tokens":0,"cache_read_input_tokens":0,"cache_creation":{"ephemeral_5m_input_tokens":0,"ephemeral_1h_input_tokens":0},"output_tokens":19,"service_tier":"batch","inference_geo":"not_available"}}}}
```

<br>

## Common issues and batch privacy

### Common issues (troubleshooting)

- Size limit exceeded: >256 MB → errors like `request_too_large`.
- Non-unique `custom_id` → ambiguity / failures.
- Invalid `params` (discovered at the end) → test with Messages API first.
- Results unavailable: more than 29 days since `created_at` (the batch may still be visible).
- Expiration: some requests may be `expired` if not sent within 24 hours.
- Retry strategy: treat `invalid_request` as "fix and resubmit" vs server errors as "retryable".

<br>

### Privacy / isolation

- **Workspace isolation:** batches and results are accessible only within the workspace tied to your API key and permissions.
- **Request independence:** no cross-request data leakage.
- **Limited retention:** results are retrievable for a fixed window (29 days), then no longer downloadable.
- Org/workspace controls may restrict Console downloads.

<br>




## Quick Selection Guide

Use the Message Batches API when your workload matches most of the following criteria:
- **High volume:** hundreds to tens of thousands of independent requests.
- **No strict latency requirement:** results can arrive minutes or hours later.
- **Cost sensitivity:** you want the built-in 50% price reduction (optionally stacked with prompt caching).
- **Embarrassingly parallel tasks:** evaluations, classification, summarization, moderation, bulk generation.
- **Loose ordering constraints:** results can be post-processed and reassembled using `custom_id`.

Avoid batches when:
- You need ***streaming*** or ***immediate responses***.
- Requests are ***tightly sequential*** or depend on previous outputs.
- You need strong guarantees on ***completion time per request***.

In short: batches optimize for ***throughput and cost***, not for ***interactivity or low latency***.

<br>




## Practical Notes

- **Design `custom_id` carefully:** treat it as a stable primary key (e.g., dataset ID, shard ID, composite keys).
- **Always assume partial failure:** production code should handle errored, expired, and canceled results explicitly.
- **Test request shapes early:** validate one representative request with the Messages API before batching at scale.
- **Stream results, don’t bulk-download:** JSONL streaming is safer and more memory-efficient for large batches.
- **Chunk very large workloads:** multiple medium-sized batches are often easier to retry, monitor, and reason about.
- **Watch spend limits:** batches may briefly exceed workspace spend caps due to high concurrency.
- **Cache aggressively but deliberately:** only cache large, truly shared context blocks; avoid caching volatile content.
