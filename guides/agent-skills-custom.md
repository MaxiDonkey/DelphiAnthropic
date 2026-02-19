# Custom Skills – API & Versioning [beta]

This document covers the **lifecycle management of custom Agent Skills** using the Anthropic Skills API.

It focuses exclusively on:
- creating and uploading custom Skills
- listing and retrieving Skills
- managing Skill versions
- deleting Skills and versions

Runtime usage, orchestration, and execution behavior are covered in [agent-skills](agent-skills.md).

- [Prerequisites](#prerequisites)
- [Custom Skill lifecycle (overview)](#custom-skill-lifecycle-overview)
- [Skill management API](#skill-management-api)
- [Skill version management API](#skill-version-management-api)
- [Using Skill versions in Messages](#using-skill-versions-in-messages)
- [Operational notes](#operational-notes)
- [References](#references)

___

<br>

## Prerequisites

Managing custom Skills requires the **Skills API beta**:

- `skills-2025-10-02`

All operations described in this document are workspace-scoped and apply only to custom Skills.

<br>

## Custom Skill lifecycle (overview)

A custom Skill follows this lifecycle:
1. **Create Skill** <br>
   Upload an initial Skill bundle (creates the Skill and its first immutable version)
2. **Use Skill** <br>
   Attach the Skill to Messages requests via container.skills
3. **Create new versions** <br>
   Upload updated Skill bundles as new versions
4. **List / retrieve** <br>
   Inspect existing Skills and versions
5. **Delete** <br>
   Remove versions, then delete the Skill itself

Anthropic-managed Skills do not support these operations.

<br>

## Skill management API

These endpoints operate on the Skill entity itself, independently of its versions. <br>
They are used to create, list, retrieve, and delete Skills.

>[!IMPORTANT]
> Manages the Skill as a logical container (identity, visibility, lifecycle root).

- [Creating a Skill](#creating-a-skill)
- [Listing Skills](#listing-skills)
- [Retrieving a Skill](#retrieving-a-skill)
- [Deleting a Skill](#deleting-a-skill)

<br>

### Creating a Skill

Creates a new custom Skill and automatically creates its first immutable version by uploading a Skill bundle.

```pascal
  var Folder := '..\media\pdf-extract';  // <--- contains the files to define the skill 

  //Multipart payload creation
  var Payload: TSkillFormDataParamProc :=
    procedure (Params: TSkillFormDataParams)
    begin
      Params
        .DisplayTitle('PDF extract')
        .Files(Folder);
    end;

  // Asynchronous example
  var Promise := Client.Skills.AsyncAwaitCreate(Payload);

  Promise
    .&Then(
      procedure (Value: TSkill)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Skill := Client.Skills.Create(Payload);
//  try
//    Display(TutorialHub, Skill);
//  finally
//    Skill.Free;
//  end;
```

#### Creation requirements
- `SKILL.md` must exist at the root of the Skill bundle
- all uploaded files must share a common root directory
- total upload size ≤ 8 MB
- YAML frontmatter constraints:
  - `name`: lowercase, hyphens only, ≤ 64 chars, no reserved words
  - `description`: non-empty, ≤ 1024 chars

<br>

### Listing Skills

Lists all Skills visible to the workspace.

This includes:
- Anthropic-managed Skills
- custom Skills

Filtering by `source=custom` restricts the result set to custom Skills only.

>[!NOTE]
> Listing Skills returns both Anthropic-managed and custom Skills by default. <br>
> Version management operations apply only to custom Skills.

```pascal
  //Query params creation
  var QueryParams: TSkillListParamProc :=
    procedure (Params: TSkillListParams)
    begin
      Params.Limit(10);
    end;

  // Asynchronous example
  var Promise := Client.Skills.AsyncAwaitList(QueryParams);

  Promise
    .&Then(
      procedure (Value: TSkillList)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous example
//  var Value := Client.Skills.List(QueryParams);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

### Retrieving a Skill

Retrieves metadata for a specific Skill, including:
- display title
- creation and update timestamps
- latest version identifier
- source (`custom` or `anthropic`)

```pascal
  // Asynchronous example
  var SkillID := 'skill_012abc'; // e.g. skill_013FzAmAqrM5o8rZdKfBtgxy

  var Promise := Client.Skills.AsyncAwaitRetrieve(SkillID);

  Promise
    .&Then(
      procedure (Value: TSkill)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous example
//  var Value := Client.Skills.Retrieve(SkillId);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

### Deleting a Skill

Deletes a Skill **only after all its versions have been deleted**.

Attempting to delete a Skill with remaining versions will fail.

```pascal
  // Asynchronous example
  var SkillID := 'skill_012abc'; // e.g. skill_013FzAmAqrM5o8rZdKfBtgxy

  // Asynchronous example
  var Promise := Client.Skills.AsyncAwaitDelete(SkillID);

  Promise
    .&Then(
      procedure (Value: TSkillDeleted)
      begin
        Display(TutorialHub, Value);
        SkillIDEdit.Text := '';
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous example
//  var Value := Client.Skills.Delete(SkillId);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end; 
```

<br>

## Skill version management API

These endpoints operate on individual versions of a custom Skill. <br>
Versions are immutable and represent deployable units.

>[!IMPORTANT]
> Manages immutable deployable snapshots associated with a Skill.

- [Version model](#version-model)
- [Creating a new Skill version](#creating-a-new-skill-version)
- [Listing Skill versions](#listing-skill-versions)
- [Retrieving a Skill version](#retrieving-a-skill-version)
- [Deleting a Skill version](#deleting-a-skill-version)

<br>

### Version model

- Each upload creates a new version
- Versions cannot be modified after creation
- Custom Skill versions use auto-generated identifiers
- Versions can be:
  - pinned explicitly
  - or referenced as `latest`

>[!NOTE]
> Versions represent deployable, immutable snapshots of a Skill bundle.

<br>

### Creating a new Skill version

Uploads an updated Skill bundle to an existing Skill, creating a new version. <br>
The Skill ID remains unchanged.

```pascal
  var Folder := '..\media\pdf-extract';

  var SkillID := 'skill_012abc'; // e.g. skill_013FzAmAqrM5o8rZdKfBtgxy

  //Multipart payload creation
  var Payload: TSkillFormDataParamProc :=
    procedure (Params: TSkillFormDataParams)
    begin
      Params
        .Files(Folder);
    end;

  // Asynchronous example
  var Promise := Client.Skills.AsyncAwaitCreate(SkillID, Payload);

  Promise
    .&Then(
      procedure (Value: TSkillVersion)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var SkillVersion := Client.Skills.Create(SkillID, version, Payload);
//  try
//    Display(TutorialHub, SkillVersion);
//  finally
//    SkillVersion.Free;
//  end; 
```

<br>

### Listing Skill versions

Lists all versions associated with a given Skill.

```pascal
  var SkillID := 'skill_012abc'; // e.g. skill_013FzAmAqrM5o8rZdKfBtgxy

  //Query params creation
  var QueryParams: TSkillListParamProc :=
    procedure (Params: TSkillListParams)
    begin
      Params.Limit(10);
    end;

  // Asynchronous example
  var Promise := Client.Skills.AsyncAwaitList(SkillID, QueryParams);

  Promise
    .&Then(
      procedure (Value: TSkillVersionList)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous example
//  var Value := Client.Skills.List(SkillID, QueryParams);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;  
```

<br>

### Retrieving a Skill version

Retrieves metadata for a specific version of a given Skill.

```pascal
  var SkillID := 'skill_012abc'; // e.g. skill_013FzAmAqrM5o8rZdKfBtgxy
  var version := '123456'; // e.g. 1771479026543447

  // Asynchronous example
  var Promise := Client.Skills.AsyncAwaitRetrieve(SkillID, version);

  Promise
    .&Then(
      procedure (Value: TSkillVersion)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous example
//  var Value := Client.Skills.Retrieve(SkillId, version);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

### Deleting a Skill version

Deletes a single version of a Skill.

```pascal
  var SkillID := 'skill_012abc'; // e.g. skill_013FzAmAqrM5o8rZdKfBtgxy
  var version := '123456'; // e.g. 1771479026543447

  // Asynchronous example
  var Promise := Client.Skills.AsyncAwaitDelete(SkillID, version);

  Promise
    .&Then(
      procedure (Value: TSkillDeleted)
      begin
        Display(TutorialHub, Value);
        VersionEdit.Text := '';
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous example
//  var Value := Client.Skills.Delete(SkillId, version);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

## Using Skill versions in Messages

When attaching a custom Skill to a Messages request, you may:
- pin a specific version (recommended for production)
- use "latest" (recommended for development)

```pascal
  var Document := '..\media\File_Search_file.pdf';
  var Base64 := TMediaCodec.EncodeBase64(Document);

  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 4096;
  var Prompt := 'Extract sensitive information from the PDF file';

  var SkillID := 'skill_012abc'; // e.g. skill_013FzAmAqrM5o8rZdKfBtgxy

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
                  .Add( Skill.CreateSkill('custom')  // <--- custom skill
                     .SkillId( SkillID )             // <--- ID of the custom skill
                     .Version('latest')              // <--- using lastest version
                  )
              )
          )
          .Messages( MessageParts
              .User( ContentParts
                 .AddText( Prompt )
                 .AddPDF(Base64)
              )
          )
          .Tools( ToolParts
              .Add( Tool.Beta.CreateCodeExecutionTool20250825 )
          );
    end;

  ...
```

>[!NOTE]
> See agent-skills.md for execution behavior, container reuse, and orchestration patterns.

<br>

## Operational notes

Before using custom Skills in production:
- Always pin explicit versions in stable environments
- Treat each version upload as a deployment
- Keep Skill bundles narrow and purpose-focused
- Maintain Skill sources in version control
- Custom Skills do not sync across Anthropic surfaces
- Beta features are not covered by Zero Data Retention (ZDR)

<br>

## References

- Skills API reference
- Skill Versions API reference
- Agent Skills overview
- Best practices for authoring Skills