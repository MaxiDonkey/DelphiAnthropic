# Models

Models are the core building blocks of the Claude API: they define the capabilities, performance, costs, and usage constraints exposed to an application.

- [Overview of the model API](#overview-of-the-model-api)
- [How to list all models](#how-to-list-all-models)
- [How to retrieve information about a model](#how-to-retrieve-information-about-a-model)

___

## Overview of the model API

The Claude API exposes a family ***of versioned models***, each identified by a stable model ID (e.g. `claude-opus-4-6`).
Each model is defined by:
- **capabilities** (reasoning, vision, extended/adaptive thinking),
- **technical constraints** (context window, maximum output, latency),
- **reliability metadata** (knowledge cutoff, training cutoff),
- *pricing* (input/output cost per MTok).

>[!IMPORTANT]
> Pricing data is exposed as metadata and may vary by platform.

Models are accessed uniformly through:
- the Anthropic API,
- AWS Bedrock,
- Google Vertex AI, with platform-specific identifiers but identical functional semantics.

<br>

## How to list all models

The list of available models is ***provider-defined*** and published in the official documentation.
In practice, models are always grouped into:
- **current models** (recommended and actively maintained),
- **legacy models** (still accessible but being phased out).

From an API perspective, models are not discovered dynamically:
they are ***explicitly referenced*** in requests (e.g. `model: "claude-opus-4-6"`).
The documentation therefore acts as the authoritative source for:
- knowing which models exist,
- identifying which ones to migrate to or avoid,
- understanding stable aliases.

<br>

### Example

The List API is primarily intended for inspection and tooling, not for model discovery guarantees.

```pascal
  // Query params creation
  var QueryParams: TListModelsParamProc :=
    procedure (Params: TListModelsParams)
    begin
      Params.Limit(10);
    end;

  // Asynchronous example
  var Promise := Client.Models.AsyncAwaitList(QueryParams);

  Promise
    .&Then(
      procedure (Value: TModels)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Value := Client.Models.List(QueryParams);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

## How to retrieve information about a model

Model information is obtained at three complementary levels:

**1. API identity**
   - model ID, alias, Bedrock / Vertex identifiers

**2. Technical specifications**
   - context window, maximum output, relative latency,
   - supported features (extended/adaptive thinking)

**3. Temporal guarantees**
   - *reliable knowledge cutoff* (what the model can reliably know),
   - *training data cutoff* (upper bound of training data)

These properties are ***snapshot-stable***:
a model with the same version/date is guaranteed to behave identically across platforms.

### Example

```pascal
  var ModelID := 'claude-opus-4-6';

  // Asynchronous example
  var Promise := Client.Models.AsyncAwaitRetrieve(ModelID);

  Promise
    .&Then(
      procedure (Value: TModel)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Value := Client.Models.Retrieve(ModelID);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

