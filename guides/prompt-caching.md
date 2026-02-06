# Prompt caching

Prompt caching optimizes API performance by reusing prompt prefixes, making repeated or partially static prompts faster and cheaper to process.

- [How to implement](#how-to-implement)
- [1-hour cache duration](#1-hour-cache-duration)
- [Cache limitations](#cache-limitations)
- [Tools Caching](#tools-caching)
- [Images Caching](#images-caching)
- [PDF Caching](#pdf-caching)
- [Continuing a multi-turn conversation](#continuing-a-multi-turn-conversation)
- [Putting it all together: Multiple cache breakpoints](#putting-it-all-together-multiple-cache-breakpoints)

___

## How to implement

Prompt Caching optimizes API usage by caching prompt prefixes, reducing processing time and costs for repetitive tasks. If a prompt prefix is cached from a recent query, it's reused; otherwise, the full prompt is processed and cached for future use. The cache lasts 5 minutes and is refreshed with each use, making it ideal for prompts with many examples, background information, or consistent instructions.

#### JSON Payload creation - system caching
```pascal
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var SystemPrompt1 := 'You are an AI assistant tasked with analyzing literary works. Your goal is to provide insightful commentary on themes, characters, and writing style.';
  var SystemPrompt2 := '<the entire contents of Pride and Prejudice>';
  var Prompt := 'Analyze the major themes in Pride and Prejudice.';

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .System( TextBlockParts
             .Add( CreateTextBlock
                 .Text(SystemPrompt1)
             )
             .Add( CreateTextBlock
                 .Text(SystemPrompt2)
                 .CacheControl( Cache
                     .AddCacheControl  // Cache enabled for 5m with system2 content
                 )
             )
          )
          .Messages( MessageParts
              .User( Prompt )
          )
          .MaxTokens(MaxTokens);
    end;
```

<br>

## 1-hour cache duration

In cases where a 5-minute cache is too limited, Anthropic offers an extended 1-hour cache option at an additional cost.
This option is enabled by explicitly setting `ttl` within `cache_control`.

>[!NOTE]
> Use the 5-minute cache for prompts that are called regularly (at least every 5 minutes), as it refreshes automatically at no additional cost.
>The 1-hour cache is better suited for less frequent prompts, latency-sensitive workflows, or to optimize rate limits (cache hits do not count against rate limits).
>Both options have similar latency characteristics, with a noticeable improvement in time to first token for long content.


#### JSON Payload creation - 1h system caching
```pascal
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var SystemPrompt1 := 'You are an AI assistant tasked with analyzing literary works. Your goal is to provide insightful commentary on themes, characters, and writing style.';
  var SystemPrompt2 := '<the entire contents of Pride and Prejudice>';
  var Prompt := 'Analyze the major themes in Pride and Prejudice.';

  StarPayload(Prompt);

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .System( TextBlockParts
              .Add( CreateTextBlock
                  .Text(SystemPrompt1)
              )
              .Add( CreateTextBlock
                  .Text(SystemPrompt2)
                  .CacheControl( Cache
                      .AddCacheControl('1h')  //'5m' for 5 minutes
                  )
              )
          )
          .Messages( MessageParts
              .User( Prompt )
          )
          .MaxTokens(MaxTokens);
    end;
```

<br>

## Cache limitations

Caching only applies to prompts that exceed a model-specific minimum token threshold. Shorter prompts are never cached, even if marked with cache_control, and are processed normally.
A cache entry becomes available only after the first response has started; for parallel requests, you must wait for that initial response before issuing subsequent ones.
Currently, only the ephemeral cache type is supported, with a default time-to-live of 5 minutes.

>[!NOTE]
> - 4096 tokens for Claude Opus 4.6 and Claude Opus 4.5
> - 1024 tokens for Claude Sonnet 4.5, Claude Opus 4.1, Claude Opus 4, Claude Sonnet 4, and Claude Sonnet 3.7 (deprecated)
> - 4096 tokens for Claude Haiku 4.5
> - 2048 tokens for Claude Haiku 3.5 (deprecated) and Claude Haiku 3

<br>

### What can be cached

Most parts of a request can be cached using cache_control, including `tool definitions`, `system messages`, `text content`, `images and documents`, as well as tool usage and tool results for both user and assistant messages.


<br>

### What cannot be cached

Some content is not directly cacheable: reasoning blocks, sub-content blocks (e.g., citations), and empty text blocks.
Reasoning blocks can still be cached indirectly via previous assistant turns, in which case they count as input tokens.
For citations, it is recommended to cache the top-level document blocks that serve as the source material.

<br>

### What invalidates the cache

Any change to cached content may partially or fully invalidate the cache.
The cache follows a `tools → system → messages` hierarchy: modifying a level invalidates that level and all subsequent levels.
A table summarizes which sections are invalidated by each type of change.

See [official documentation](https://platform.claude.com/docs/en/build-with-claude/prompt-caching#what-invalidates-the-cache)

<br>

## Tools Caching

- Set parameters
  ```pascal
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 1024;
    var Prompt := 'What is the weather and time in New York?';

    var WeatherFunctionName := 'get_weather';
    var WeatherFunctionDescription := 'Get the current weather in a given location';
    var GetWeather :=
      '''
      {
          "type": "object",
          "properties": {
              "location": {
                  "type": "string",
                  "description": "The city and state, e.g. San Francisco, CA"
              },
              "unit": {
                  "type": "string",
                  "enum": ["celsius", "fahrenheit"],
                  "description": "The unit of temperature, either celsius or fahrenheit"
              }
          },
          "required": ["location"]
      }
      ''';

    var TimeFunctionName := 'get_time';
    var TimeFunctionDescription := 'Get the current time in a given time zone';
    var GetTime :=
      '''
      {
          "type": "object",
          "properties": {
              "timezone": {
                  "type": "string",
                  "description": "The IANA time zone name, e.g. America/Los_Angeles"
              }
          },
          "required": ["timezone"]
      }
      ''';
   ```
 
- JSON Payload creation - tools caching
  ```pascal
    //JSON payload Creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Tools( ToolParts
                .Add( Tool.CreateToolCustom
                    .Name( WeatherFunctionName )
                    .Description( WeatherFunctionDescription )
                    .InputSchema( GetWeather )
                )
                // many more tools
                .Add( Tool.CreateToolCustom
                    .Name( TimeFunctionName )
                    .Description( TimeFunctionDescription )
                    .InputSchema( GetTime )
                    .CacheControl( Cache   
                        .AddCacheControl  //Tool caching '5m' by default
                    )
                )
            )
            .Messages( MessageParts
                .User( Prompt )
            )
      end;
  ```

- JSON Result
  ```json
  {
        "model": "claude-opus-4-6",
        "max_tokens": 1024,
        "tools": [
            {
                "type": "custom",
                "name": "get_weather",
                "description": "Get the current weather in a given location",
                "input_schema": {
                    "type": "object",
                    "properties": {
                        "location": {
                            "type": "string",
                            "description": "The city and state, e.g. San Francisco, CA"
                        },
                        "unit": {
                            "type": "string",
                            "enum": [
                                "celsius",
                                "fahrenheit"
                            ],
                            "description": "The unit of temperature, either celsius or fahrenheit"
                        }
                    },
                    "required": [
                        "location"
                    ]
                }
            },
            {
                "type": "custom",
                "name": "get_time",
                "description": "Get the current time in a given time zone",
                "input_schema": {
                    "type": "object",
                    "properties": {
                        "timezone": {
                            "type": "string",
                            "description": "The IANA time zone name, e.g. America\/Los_Angeles"
                        }
                    },
                    "required": [
                        "timezone"
                    ]
                },
                "cache_control": {
                    "type": "ephemeral"
                }
            }
        ],
        "messages": [
            {
                "role": "user",
                "content": "What is the weather and time in New York?"
            }
        ]
    }        
  ```

<br>

## Images Caching

- Set parameters
  ```pascal
    var Document := '..\..\..\media\MyImage.png';  // The image size must be at least 4096 to be cached.
    var Base64 := TMediaCodec.EncodeBase64(Document);
    var MimeType := TMediaCodec.GetMimeType(Document);

    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 1024;
    var Prompt := 'What is in the above image?';
  ```

- JSON Payload creation - image caching
  ```pascal
    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .Messages( MessageParts
                .User( ContentParts
                    .AddImage( CreateImageBlock
                        .Source(Base64, MimeType)
                        .CacheControl( Cache  
                            .AddCacheControl  //Image caching '5m' by default
                        )
                    )
                    .AddText(Prompt)
                )
            )
            .MaxTokens(MaxTokens)
            .Stream;
      end;  
  ```

- JSON Result
  ```json
      {
        "model": "claude-opus-4-6",
        "messages": [
            {
                "role": "user",
                "content": [
                    {
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "data": "iVBORw0KGgoAAAANSUhEUgAAArwAAAPTCAIAAADdD...RMHCUFI7UYlKhQFfEJ9AKMogk6KsB\\\/",
                            "media_type": "image\/png"
                        },
                        "cache_control": {
                            "type": "ephemeral"
                        }
                    },
                    {
                        "type": "text",
                        "text": "What is in the above image?"
                    }
                ]
            }
        ],
        "max_tokens": 1024,
        "stream": true
    }  
  ```

<br>

## PDF Caching

- Set parameters
  ```pascal
    var Document := '..\..\..\media\File_Search_file.pdf'; // The document size must be at least 4096 to be cached.
    var Base64 := TMediaCodec.EncodeBase64(Document);

    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 1024;
    var Prompt := 'How can the work documented in the PDF be relevant?';
  ```

- JSON Payload creation - document caching
  ```pascal
    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .Messages( MessageParts
                .User( ContentParts
                    .AddPDF( BlockContent
                        .AddPDF( Base64 )
                        .CacheControl( Cache  
                            .AddCacheControl  //Document caching '5m' by default
                        )
                    )
                    .AddText(Prompt)
                )
            )
            .MaxTokens(MaxTokens)
            .Stream;
      end;
  ```

- JSON Result
  ```json
  {
        "model": "claude-opus-4-6",
        "messages": [
            {
                "role": "user",
                "content": [
                    {
                        "type": "document",
                        "source": {
                            "type": "base64",
                            "data": "JVBERi0xLjcNCiW1tbW1DQoxIDAgb2JqDQo8PC...X7b8decnrXtAh58GFVxonuPh\\\/MV06WrwxPO",
                            "media_type": "application\/pdf"
                        },
                        "cache_control": {
                            "type": "ephemeral"
                        }
                    },
                    {
                        "type": "text",
                        "text": "How can the work documented in the PDF be relevant?"
                    }
                ]
            }
        ],
        "max_tokens": 1024,
        "stream": true
    }
  ```

<br>

## Continuing a multi-turn conversation

- Set parameters
  ```pascal
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 1024;
    var SystemPrompt := '...long system prompt';
  ```

- JSON Payload creation - document caching
  ```pascal
    //JSON payload creation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .System( TextBlockParts
                  .Add( CreateTextBlock
                      .Text(SystemPrompt)
                      .CacheControl( Cache
                          .AddCacheControl
                      )
                  )
              )
            .Messages( MessageParts
                .User( ContentParts
                    .AddText('Hello, can you tell me more about the solar system?'
                    )
                )
                .Assistant('Certainly! The solar system is the collection of celestial bodies that orbit our Sun. It consists of eight planets, numerous moons, asteroids, comets, and other objects. The planets, in order from closest to farthest from the Sun, are: Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, and Neptune. Each planet has its own unique characteristics and features. Is there a specific aspect of the solar system you would like to know more about?'
                )
                .User( ContentParts
                    .AddText('Good to know.')
                    .AddText( CreateTextBlock
                        .Text('Tell me more about Mars.')
                        .CacheControl( Cache
                            .AddCacheControl  //text caching '5m' by default
                        )
                    )
                )
            );
      end;
  ```

- JSON Result
  ```json
  {
          "model": "claude-opus-4-6",
          "max_tokens": 1024,
          "system": [
              {
                  "type": "text",
                  "text": "...long system prompt",
                  "cache_control": {"type": "ephemeral"}
              }
          ],
          "messages": [
              {
                  "role": "user",
                  "content": [
                      {
                          "type": "text",
                          "text": "Hello, can you tell me more about the solar system?",
                      }
                  ]
              },
              {
                  "role": "assistant",
                  "content": "Certainly! The solar system is the collection of celestial bodies that orbit our Sun. It consists of eight planets, numerous moons, asteroids, comets, and other objects. The planets, in order from closest to farthest from the Sun, are: Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, and Neptune. Each planet has its own unique characteristics and features. Is there a specific aspect of the solar system you would like to know more about?"
              },
              {
                  "role": "user",
                  "content": [
                      {
                          "type": "text",
                          "text": "Good to know."
                      },
                      {
                          "type": "text",
                          "text": "Tell me more about Mars.",
                          "cache_control": {"type": "ephemeral"}
                      }
                  ]
              }
          ]
      }
  ```

<br>

## Putting it all together: Multiple cache breakpoints

