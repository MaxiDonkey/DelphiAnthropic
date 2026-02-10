unit Anthropic.Skills;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  REST.JsonReflect, REST.Json.Types,
  Anthropic.API.Params, Anthropic.API, Anthropic.Types,
  Anthropic.Async.Support, Anthropic.Async.Promise;

type
  TSkillListParams = class(TUrlParam)
  public
    /// <summary>
    /// Number of results to return per page.
    /// </summary>
    /// <param name="Value">
    /// Defaults to 20. Ranges from 1 to 100.
    /// </param>
    function Limit(const Value: Integer): TSkillListParams;

    /// <summary>
    /// Pagination token for fetching a specific page of results.
    /// </summary>
    /// <remarks>
    /// Pass the value from a previous response's next_page field to get the next page of results.
    /// </remarks>
    function Page(const Value: string): TSkillListParams;

    /// <summary>
    /// Filter skills by source.
    /// </summary>
    /// <remarks>
    /// If provided, only skills from the specified source will be returned:
    /// <para>
    /// • "custom": only return user-created skills
    /// </para>
    /// <para>
    /// • "anthropic": only return Anthropic-created skills
    /// </para>
    /// </remarks>
    function Source(const Value: string): TSkillListParams;
  end;

  TSkill = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('display_title')]
    FDisplayTitle: string;
    [JsonNameAttribute('latest_version')]
    FLatestVersion: string;
    FSource: string;
    FType: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
  public
    /// <summary>
    /// Unique identifier for the skill.
    /// </summary>
    /// <remarks>
    /// The format and length of IDs may change over time.
    /// </remarks>
    property Id: string read FId write FId;

    /// <summary>
    /// ISO 8601 timestamp of when the skill was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Display title for the skill.
    /// </summary>
    /// <remarks>
    /// This is a human-readable label that is not included in the prompt sent to the model.
    /// </remarks>
    property DisplayTitle: string read FDisplayTitle write FDisplayTitle;

    /// <summary>
    /// The latest version identifier for the skill.
    /// </summary>
    /// <remarks>
    /// This represents the most recent version of the skill that has been created.
    /// </remarks>
    property LatestVersion: string read FLatestVersion write FLatestVersion;

    /// <summary>
    /// Source of the skill.
    /// </summary>
    /// <remarks>
    /// This may be one of the following values:
    /// <para>
    /// • "custom": the skill was created by a user
    /// </para>
    /// <para>
    /// • "anthropic": the skill was created by Anthropic
    /// </para>
    /// </remarks>
    property Source: string read FSource write FSource;

    /// <summary>
    /// For Skills, this is always "skill".
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// ISO 8601 timestamp of when the skill was last updated.
    /// </summary>
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;
  end;

  TSkillList = class(TJSONFingerprint)
  private
    FData: TArray<TSkill>;
    [JsonNameAttribute('has_more')]
    FHasMore: Boolean;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  public
    /// <summary>
    /// List of skills.
    /// </summary>
    property Data: TArray<TSkill> read FData write FData;

    /// <summary>
    /// Whether there are more results available.
    /// </summary>
    /// <remarks>
    /// If true, there are additional results that can be fetched using the next_page token.
    /// </remarks>
    property HasMore: Boolean read FHasMore write FHasMore;

    /// <summary>
    /// Token for fetching the next page of results.
    /// </summary>
    /// <remarks>
    /// If null, there are no more results available. Pass this value to the page_token parameter
    /// in the next request to get the next page.
    /// </remarks>
    property NextPage: string read FNextPage write FNextPage;

    destructor Destroy; override;
  end;

  TSkillDeleted = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
  public
    /// <summary>
    /// Unique identifier for the skill.
    /// </summary>
    /// <remarks>
    /// The format and length of IDs may change over time.
    /// </remarks>
    property Id: string read FId write FId;

    /// <summary>
    /// For Skills, this is always "skill_deleted".
    /// </summary>
    property &Type: string read FType write FType;
  end;

  /// <summary>
  /// Defines an asynchronous callback handler for skill operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynSkill</c> is a type alias for <c>TAsynCallBack&lt;TSkill&gt;</c>, specialized for receiving
  /// <c>TSkill</c> results from asynchronous skill route calls.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous callback infrastructure to the Skills domain by fixing the
  /// result type to <c>TSkill</c>.
  /// </para>
  /// <para>
  /// • This alias improves API readability and intent clarity while preserving the complete behavior
  /// and lifecycle semantics of <c>TAsynCallBack&lt;TSkill&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TAsynSkill</c> for callbacks associated with skill create and retrieve operations.
  /// </para>
  /// </remarks>
  TAsynSkill = TAsynCallBack<TSkill>;

  /// <summary>
  /// Defines a promise-based callback handler for skill operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseSkill</c> is a type alias for <c>TPromiseCallback&lt;TSkill&gt;</c>, specialized for
  /// receiving <c>TSkill</c> results from promise-oriented skill route calls.
  /// </para>
  /// <para>
  /// • It binds the generic promise-based callback infrastructure to the Skills domain by fixing the
  /// result type to <c>TSkill</c>.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing promise-based consumption semantics
  /// while preserving the complete behavior of <c>TPromiseCallback&lt;TSkill&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseSkill</c> when consuming skill creation, retrieval, and listing operations
  /// through promise-oriented abstractions instead of direct callbacks.
  /// </para>
  /// </remarks>
  TPromiseSkill = TPromiseCallback<TSkill>;

  /// <summary>
  /// Defines an asynchronous callback handler for skill list operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynSkillList</c> is a type alias for <c>TAsynCallBack&lt;TSkillList&gt;</c>, specialized for
  /// receiving <c>TSkillList</c> results from asynchronous skill listing route calls.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous callback infrastructure to the Skills domain by fixing the
  /// result type to <c>TSkillList</c>.
  /// </para>
  /// <para>
  /// • This alias improves API readability and intent clarity while preserving the complete behavior
  /// and lifecycle semantics of <c>TAsynCallBack&lt;TSkillList&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TAsynSkillList</c> for callbacks associated with skill enumeration and pagination
  /// operations.
  /// </para>
  /// </remarks>
  TAsynSkillList = TAsynCallBack<TSkillList>;

  /// <summary>
  /// Defines a promise-based callback handler for skill list operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseSkillList</c> is a type alias for <c>TPromiseCallback&lt;TSkillList&gt;</c>, specialized
  /// for receiving <c>TSkillList</c> results from promise-oriented skill listing route calls.
  /// </para>
  /// <para>
  /// • It binds the generic promise-based callback infrastructure to the Skills domain by fixing the
  /// result type to <c>TSkillList</c>.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing promise-based consumption semantics
  /// while preserving the complete behavior of <c>TPromiseCallback&lt;TSkillList&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseSkillList</c> when consuming skill enumeration and pagination operations through
  /// promise-oriented abstractions instead of direct callbacks.
  /// </para>
  /// </remarks>
  TPromiseSkillList = TPromiseCallback<TSkillList>;

  /// <summary>
  /// Defines an asynchronous callback handler for skill deletion operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynSkillDeleted</c> is a type alias for <c>TAsynCallBack&lt;TSkillDeleted&gt;</c>, specialized
  /// for receiving <c>TSkillDeleted</c> results from asynchronous skill deletion route calls.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous callback infrastructure to the Skills domain by fixing the
  /// result type to <c>TSkillDeleted</c>.
  /// </para>
  /// <para>
  /// • This alias improves API readability and intent clarity while preserving the complete behavior
  /// and lifecycle semantics of <c>TAsynCallBack&lt;TSkillDeleted&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TAsynSkillDeleted</c> for callbacks associated with skill deletion operations, including
  /// version-specific deletions.
  /// </para>
  /// </remarks>
  TAsynSkillDeleted = TAsynCallBack<TSkillDeleted>;

  /// <summary>
  /// Defines a promise-based callback handler for skill deletion operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseSkillDeleted</c> is a type alias for <c>TPromiseCallback&lt;TSkillDeleted&gt;</c>,
  /// specialized for receiving <c>TSkillDeleted</c> results from promise-oriented skill deletion
  /// route calls.
  /// </para>
  /// <para>
  /// • It binds the generic promise-based callback infrastructure to the Skills domain by fixing the
  /// result type to <c>TSkillDeleted</c>.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing promise-based consumption semantics
  /// while preserving the complete behavior of <c>TPromiseCallback&lt;TSkillDeleted&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseSkillDeleted</c> when consuming skill deletion operations, including
  /// version-specific deletions, through promise-oriented abstractions instead of direct callbacks.
  /// </para>
  /// </remarks>
  TPromiseSkillDeleted = TPromiseCallback<TSkillDeleted>;

  TAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function Create: TSkill; overload; virtual; abstract;

    function Create(const SkillId: string): TSkill; overload; virtual; abstract;

    function Delete(const SkillId: string): TSkillDeleted; overload; virtual; abstract;

    function Delete(const SkillId: string; const Version: string): TSkillDeleted; overload; virtual; abstract;

    function List: TSkillList; overload; virtual; abstract;

    function List(const SkillId: string): TSkillList; overload; virtual; abstract;

    function List(const ParamProc: TProc<TSkillListParams>): TSkillList; overload; virtual; abstract;

    function List(const SkillId: string; const ParamProc: TProc<TSkillListParams>): TSkillList; overload; virtual; abstract;

    function Retrieve(const SkillId: string): TSkill; overload; virtual; abstract;

    function Retrieve(const SkillId: string; const Version: string): TSkill; overload; virtual; abstract;
  end;

  TAsynchronousSupport = class(TAbstractSupport)
  protected
    procedure AsynCreate(const CallBacks: TFunc<TAsynSkill>); overload;

    procedure AsynCreate(
      const SkillId: string;
      const CallBacks: TFunc<TAsynSkill>); overload;

    procedure AsynDelete(
      const SkillId: string;
      const CallBacks: TFunc<TAsynSkillDeleted>); overload;

    procedure AsynDelete(
      const SkillId: string;
      const Version: string;
      const CallBacks: TFunc<TAsynSkillDeleted>); overload;

    procedure AsynList(
      const CallBacks: TFunc<TAsynSkillList>); overload;

    procedure AsynList(
      const SkillId: string;
      const CallBacks: TFunc<TAsynSkillList>); overload;

    procedure AsynList(
      const ParamProc: TProc<TSkillListParams>;
      const CallBacks: TFunc<TAsynSkillList>); overload;

    procedure AsynList(
      const SkillId: string;
      const ParamProc: TProc<TSkillListParams>;
      const CallBacks: TFunc<TAsynSkillList>); overload;

    procedure AsynRetrieve(
      const SkillId: string;
      const CallBacks: TFunc<TAsynSkill>); overload;

    procedure AsynRetrieve(
      const SkillId: string;
      const Version: string;
      const CallBacks: TFunc<TAsynSkill>); overload;
  end;

  TSkillsRoute = class(TAsynchronousSupport)
  public
    /// <summary>
    /// Creates a new skill.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method creates a new skill resource using default parameters and server-side defaults.
    /// </para>
    /// <para>
    /// • The created skill is immediately persisted and becomes available for subsequent retrieval,
    /// listing, and versioning operations.
    /// </para>
    /// <para>
    /// • This overload does not require an explicit skill identifier; the identifier is generated by
    /// the service.
    /// </para>
    /// <para>
    /// • Use this method when creating a new skill without predefining its identifier.
    /// </para>
    /// </remarks>
    function Create: TSkill; overload; override;

    /// <summary>
    /// Creates a new skill with an explicit identifier.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method creates a new skill resource using the provided skill identifier.
    /// </para>
    /// <para>
    /// • The specified identifier is used as the stable reference for subsequent operations such as
    /// retrieval, listing of versions, and deletion.
    /// </para>
    /// <para>
    /// • The created skill is immediately persisted and becomes available for version management.
    /// </para>
    /// <para>
    /// • Use this overload when you need to control or predefine the skill identifier.
    /// </para>
    /// </remarks>
    function Create(const SkillId: string): TSkill; overload; override;

    /// <summary>
    /// Deletes a skill.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method deletes the specified skill and all of its associated versions.
    /// </para>
    /// <para>
    /// • Once deleted, the skill and its versions are no longer available for retrieval or listing.
    /// </para>
    /// <para>
    /// • The deletion is permanent and cannot be undone.
    /// </para>
    /// <para>
    /// • Use this overload when you need to remove an entire skill identified by its identifier.
    /// </para>
    /// </remarks>
    function Delete(const SkillId: string): TSkillDeleted; overload; override;

    /// <summary>
    /// Deletes a specific version of a skill.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method deletes a single version of the specified skill while leaving other versions intact.
    /// </para>
    /// <para>
    /// • Only the version identified by the provided version identifier is removed.
    /// </para>
    /// <para>
    /// • Once deleted, the specified version is no longer available for retrieval.
    /// </para>
    /// <para>
    /// • Use this overload when you need to remove a particular version of a skill.
    /// </para>
    /// </remarks>
    function Delete(const SkillId: string; const Version: string): TSkillDeleted; overload; override;

    /// <summary>
    /// Retrieves the list of skills.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method returns a paginated list of all accessible skills.
    /// </para>
    /// <para>
    /// • The returned list may include both user-created and Anthropic-provided skills.
    /// </para>
    /// <para>
    /// • Pagination behavior follows server defaults when no parameters are provided.
    /// </para>
    /// <para>
    /// • Use this overload when you need to enumerate skills without applying filters or constraints.
    /// </para>
    /// </remarks>
    function List: TSkillList; overload; override;

    /// <summary>
    /// Retrieves the list of versions for a specific skill.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method returns a paginated list of all versions associated with the specified skill.
    /// </para>
    /// <para>
    /// • Each entry in the result represents a distinct version of the skill.
    /// </para>
    /// <para>
    /// • Pagination behavior follows server defaults when no additional parameters are provided.
    /// </para>
    /// <para>
    /// • Use this overload when you need to enumerate versions of a particular skill.
    /// </para>
    /// </remarks>
    function List(const SkillId: string): TSkillList; overload; override;

    /// <summary>
    /// Retrieves the list of skills with query parameters applied.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method returns a paginated list of skills using the provided parameter configuration.
    /// </para>
    /// <para>
    /// • The supplied parameter procedure can be used to control pagination, filtering, and source
    /// selection.
    /// </para>
    /// <para>
    /// • Pagination and filtering behavior are applied server-side based on the configured parameters.
    /// </para>
    /// <para>
    /// • Use this overload when you need to enumerate skills with explicit constraints or filters.
    /// </para>
    /// </remarks>
    function List(const ParamProc: TProc<TSkillListParams>): TSkillList; overload; override;

    /// <summary>
    /// Retrieves the list of versions for a specific skill with query parameters applied.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method returns a paginated list of skill versions associated with the specified skill,
    /// using the provided parameter configuration.
    /// </para>
    /// <para>
    /// • The supplied parameter procedure can be used to control pagination and filtering of versions.
    /// </para>
    /// <para>
    /// • Pagination behavior is applied server-side based on the configured parameters.
    /// </para>
    /// <para>
    /// • Use this overload when you need to enumerate skill versions with explicit constraints or
    /// pagination settings.
    /// </para>
    /// </remarks>
    function List(const SkillId: string; const ParamProc: TProc<TSkillListParams>): TSkillList; overload; override;

    /// <summary>
    /// Retrieves a skill by its identifier.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method returns the current representation of the specified skill.
    /// </para>
    /// <para>
    /// • The returned skill corresponds to the latest version of the skill.
    /// </para>
    /// <para>
    /// • The skill must exist and be accessible; otherwise an error is returned by the service.
    /// </para>
    /// <para>
    /// • Use this overload when you need to retrieve a skill without specifying a version.
    /// </para>
    /// </remarks>
    function Retrieve(const SkillId: string): TSkill; overload; override;

    /// <summary>
    /// Retrieves a specific version of a skill.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method returns the representation of the specified skill version.
    /// </para>
    /// <para>
    /// • Only the version identified by the provided version identifier is retrieved.
    /// </para>
    /// <para>
    /// • The skill and version must exist and be accessible; otherwise an error is returned by the service.
    /// </para>
    /// <para>
    /// • Use this overload when you need to retrieve a precise version of a skill.
    /// </para>
    /// </remarks>
    function Retrieve(const SkillId: string; const Version: string): TSkill; overload; override;

    /// <summary>
    /// Asynchronously creates a new skill using promise-based semantics.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates asynchronous creation of a new skill and returns a promise that
    /// resolves to the created <c>TSkill</c> instance.
    /// </para>
    /// <para>
    /// • The optional callbacks parameter can be used to observe the promise lifecycle events.
    /// </para>
    /// <para>
    /// • This overload does not require an explicit skill identifier; the identifier is generated by
    /// the service.
    /// </para>
    /// <para>
    /// • Use this method when integrating skill creation into promise-based asynchronous workflows.
    /// </para>
    /// </remarks>
    function AsyncAwaitCreate(
      const Callbacks: TFunc<TPromiseSkill> = nil): TPromise<TSkill>; overload;

    /// <summary>
    /// Asynchronously creates a new skill with an explicit identifier using promise-based semantics.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates asynchronous creation of a new skill using the provided skill identifier
    /// and returns a promise that resolves to the created <c>TSkill</c> instance.
    /// </para>
    /// <para>
    /// • The specified identifier is used as the stable reference for subsequent operations on the skill.
    /// </para>
    /// <para>
    /// • The optional callbacks parameter can be used to observe the promise lifecycle events.
    /// </para>
    /// <para>
    /// • Use this overload when you need to control the skill identifier in a promise-based workflow.
    /// </para>
    /// </remarks>
    function AsyncAwaitCreate(
      const SkillId: string;
      const Callbacks: TFunc<TPromiseSkill> = nil): TPromise<TSkill>; overload;

    /// <summary>
    /// Asynchronously deletes a skill using promise-based semantics.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates asynchronous deletion of the specified skill and returns a promise that
    /// resolves to a <c>TSkillDeleted</c> result.
    /// </para>
    /// <para>
    /// • The deletion removes the skill and all of its associated versions.
    /// </para>
    /// <para>
    /// • The optional callbacks parameter can be used to observe the promise lifecycle events.
    /// </para>
    /// <para>
    /// • Use this overload when deleting an entire skill within a promise-based asynchronous workflow.
    /// </para>
    /// </remarks>
    function AsyncAwaitDelete(
      const SkillId: string;
      const Callbacks: TFunc<TPromiseSkillDeleted> = nil): TPromise<TSkillDeleted>; overload;

    /// <summary>
    /// Asynchronously deletes a specific version of a skill using promise-based semantics.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates asynchronous deletion of a single version of the specified skill and
    /// returns a promise that resolves to a <c>TSkillDeleted</c> result.
    /// </para>
    /// <para>
    /// • Only the version identified by the provided version identifier is removed.
    /// </para>
    /// <para>
    /// • Other versions of the skill remain unaffected by this operation.
    /// </para>
    /// <para>
    /// • Use this overload when deleting a particular skill version within a promise-based workflow.
    /// </para>
    /// </remarks>
    function AsyncAwaitDelete(
      const SkillId: string;
      const Version: string;
      const Callbacks: TFunc<TPromiseSkillDeleted> = nil): TPromise<TSkillDeleted>; overload;

    /// <summary>
    /// Asynchronously retrieves the list of skills using promise-based semantics.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates asynchronous retrieval of the skill list and returns a promise that
    /// resolves to a <c>TSkillList</c> instance.
    /// </para>
    /// <para>
    /// • The returned list may include both user-created and Anthropic-provided skills.
    /// </para>
    /// <para>
    /// • Pagination behavior follows server defaults when no parameters are provided.
    /// </para>
    /// <para>
    /// • Use this overload when enumerating skills within promise-based asynchronous workflows.
    /// </para>
    /// </remarks>
    function AsyncAwaitList(
      const Callbacks: TFunc<TPromiseSkillList> = nil): TPromise<TSkillList>; overload;

    /// <summary>
    /// Asynchronously retrieves the list of versions for a specific skill using promise-based semantics.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates asynchronous retrieval of all versions associated with the specified
    /// skill and returns a promise that resolves to a <c>TSkillList</c> instance.
    /// </para>
    /// <para>
    /// • Each entry in the result represents a distinct version of the skill.
    /// </para>
    /// <para>
    /// • Pagination behavior follows server defaults when no additional parameters are provided.
    /// </para>
    /// <para>
    /// • Use this overload when enumerating skill versions within promise-based asynchronous workflows.
    /// </para>
    /// </remarks>
    function AsyncAwaitList(
      const SkillId: string;
      const Callbacks: TFunc<TPromiseSkillList> = nil): TPromise<TSkillList>; overload;

    /// <summary>
    /// Asynchronously retrieves the list of skills with query parameters applied using promise-based semantics.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates asynchronous retrieval of the skill list using the provided parameter
    /// configuration and returns a promise that resolves to a <c>TSkillList</c> instance.
    /// </para>
    /// <para>
    /// • The supplied parameter procedure can be used to control pagination, filtering, and source
    /// selection.
    /// </para>
    /// <para>
    /// • Pagination and filtering behavior are applied server-side based on the configured parameters.
    /// </para>
    /// <para>
    /// • Use this overload when enumerating skills with explicit constraints in promise-based workflows.
    /// </para>
    /// </remarks>
    function AsyncAwaitList(
      const ParamProc: TProc<TSkillListParams>;
      const Callbacks: TFunc<TPromiseSkillList> = nil): TPromise<TSkillList>; overload;

    /// <summary>
    /// Asynchronously retrieves the list of versions for a specific skill with query parameters applied
    /// using promise-based semantics.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates asynchronous retrieval of skill versions associated with the specified
    /// skill using the provided parameter configuration and returns a promise that resolves to a
    /// <c>TSkillList</c> instance.
    /// </para>
    /// <para>
    /// • The supplied parameter procedure can be used to control pagination and filtering of versions.
    /// </para>
    /// <para>
    /// • Pagination and filtering behavior are applied server-side based on the configured parameters.
    /// </para>
    /// <para>
    /// • Use this overload when enumerating skill versions with explicit constraints in promise-based
    /// workflows.
    /// </para>
    /// </remarks>
    function AsyncAwaitList(
      const SkillId: string;
      const ParamProc: TProc<TSkillListParams>;
      const Callbacks: TFunc<TPromiseSkillList> = nil): TPromise<TSkillList>; overload;

    /// <summary>
    /// Asynchronously retrieves a skill by its identifier using promise-based semantics.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates asynchronous retrieval of the specified skill and returns a promise that
    /// resolves to a <c>TSkill</c> instance.
    /// </para>
    /// <para>
    /// • The returned skill corresponds to the latest version of the skill.
    /// </para>
    /// <para>
    /// • The optional callbacks parameter can be used to observe the promise lifecycle events.
    /// </para>
    /// <para>
    /// • Use this overload when retrieving a skill in promise-based asynchronous workflows.
    /// </para>
    /// </remarks>
    function AsyncAwaitRetrieve(
      const SkillId: string;
      const Callbacks: TFunc<TPromiseSkill> = nil): TPromise<TSkill>; overload;

    /// <summary>
    /// Asynchronously retrieves a specific version of a skill using promise-based semantics.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates asynchronous retrieval of the specified skill version and returns a
    /// promise that resolves to a <c>TSkill</c> instance.
    /// </para>
    /// <para>
    /// • Only the version identified by the provided version identifier is retrieved.
    /// </para>
    /// <para>
    /// • The skill and version must exist and be accessible; otherwise an error is returned by the
    /// service.
    /// </para>
    /// <para>
    /// • Use this overload when retrieving a precise skill version in promise-based asynchronous
    /// workflows.
    /// </para>
    /// </remarks>
    function AsyncAwaitRetrieve(
      const SkillId: string;
      const Version: string;
      const Callbacks: TFunc<TPromiseSkill> = nil): TPromise<TSkill>; overload;
  end;

implementation

{ TSkillsRoute }

function TSkillsRoute.AsyncAwaitCreate(
  const Callbacks: TFunc<TPromiseSkill>): TPromise<TSkill>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSkill>(
    procedure(const CallbackParams: TFunc<TAsynSkill>)
    begin
      Self.AsynCreate(CallbackParams);
    end,
    Callbacks);
end;

function TSkillsRoute.AsyncAwaitCreate(const SkillId: string;
  const Callbacks: TFunc<TPromiseSkill>): TPromise<TSkill>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSkill>(
    procedure(const CallbackParams: TFunc<TAsynSkill>)
    begin
      Self.AsynCreate(SkillId, CallbackParams);
    end,
    Callbacks);
end;

function TSkillsRoute.AsyncAwaitDelete(const SkillId: string;
  const Callbacks: TFunc<TPromiseSkillDeleted>): TPromise<TSkillDeleted>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSkillDeleted>(
    procedure(const CallbackParams: TFunc<TAsynSkillDeleted>)
    begin
      Self.AsynDelete(SkillId, CallbackParams);
    end,
    Callbacks);
end;

function TSkillsRoute.AsyncAwaitDelete(const SkillId, Version: string;
  const Callbacks: TFunc<TPromiseSkillDeleted>): TPromise<TSkillDeleted>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSkillDeleted>(
    procedure(const CallbackParams: TFunc<TAsynSkillDeleted>)
    begin
      Self.AsynDelete(SkillId, Version, CallbackParams);
    end,
    Callbacks);
end;

function TSkillsRoute.AsyncAwaitList(
  const Callbacks: TFunc<TPromiseSkillList>): TPromise<TSkillList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSkillList>(
    procedure(const CallbackParams: TFunc<TAsynSkillList>)
    begin
      Self.AsynList(CallbackParams);
    end,
    Callbacks);
end;

function TSkillsRoute.AsyncAwaitList(const SkillId: string;
  const Callbacks: TFunc<TPromiseSkillList>): TPromise<TSkillList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSkillList>(
    procedure(const CallbackParams: TFunc<TAsynSkillList>)
    begin
      Self.AsynList(SkillId, CallbackParams);
    end,
    Callbacks);
end;

function TSkillsRoute.AsyncAwaitList(const ParamProc: TProc<TSkillListParams>;
  const Callbacks: TFunc<TPromiseSkillList>): TPromise<TSkillList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSkillList>(
    procedure(const CallbackParams: TFunc<TAsynSkillList>)
    begin
      Self.AsynList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSkillsRoute.AsyncAwaitList(const SkillId: string;
  const ParamProc: TProc<TSkillListParams>;
  const Callbacks: TFunc<TPromiseSkillList>): TPromise<TSkillList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSkillList>(
    procedure(const CallbackParams: TFunc<TAsynSkillList>)
    begin
      Self.AsynList(SkillId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSkillsRoute.AsyncAwaitRetrieve(const SkillId: string;
  const Callbacks: TFunc<TPromiseSkill>): TPromise<TSkill>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSkill>(
    procedure(const CallbackParams: TFunc<TAsynSkill>)
    begin
      Self.AsynRetrieve(SkillId, CallbackParams);
    end,
    Callbacks);
end;

function TSkillsRoute.AsyncAwaitRetrieve(const SkillId, Version: string;
  const Callbacks: TFunc<TPromiseSkill>): TPromise<TSkill>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSkill>(
    procedure(const CallbackParams: TFunc<TAsynSkill>)
    begin
      Self.AsynRetrieve(SkillId, Version, CallbackParams);
    end,
    Callbacks);
end;

function TSkillsRoute.Create: TSkill;
begin
  Result := API.Post<TSkill>('skills');
end;

function TSkillsRoute.Create(const SkillId: string): TSkill;
begin
  Result := API.Post<TSkill>('skills/' + SkillId);
end;

function TSkillsRoute.Delete(const SkillId: string): TSkillDeleted;
begin
  Result := API.Delete<TSkillDeleted>('skills/' + SkillId);
end;

function TSkillsRoute.Delete(const SkillId, Version: string): TSkillDeleted;
begin
  Result := API.Delete<TSkillDeleted>('skills/' + SkillId + '/versions/' + Version);
end;

function TSkillsRoute.List: TSkillList;
begin
  Result := API.Get<TSkillList>('skills');
end;

function TSkillsRoute.List(const SkillId: string): TSkillList;
begin
  Result := API.Get<TSkillList>('skills/' + SkillId + '/versions');
end;

function TSkillsRoute.List(
  const ParamProc: TProc<TSkillListParams>): TSkillList;
begin
  Result := API.Get<TSkillList, TSkillListParams>('skills', ParamProc);
end;

function TSkillsRoute.List(const SkillId: string;
  const ParamProc: TProc<TSkillListParams>): TSkillList;
begin
  Result := API.Get<TSkillList, TSkillListParams>('skills/' + SkillId + '/versions', ParamProc);
end;

function TSkillsRoute.Retrieve(const SkillId, Version: string): TSkill;
begin
  Result := API.Get<TSkill>('skills/' + SkillId + '/versions/' + Version);
end;

function TSkillsRoute.Retrieve(const SkillId: string): TSkill;
begin
  Result := API.Get<TSkill>('skills/' + SkillId);
end;

{ TSkillListParams }

function TSkillListParams.Limit(const Value: Integer): TSkillListParams;
begin
  Result := TSkillListParams(Add('limit', Value));
end;

function TSkillListParams.Page(const Value: string): TSkillListParams;
begin
  Result := TSkillListParams(Add('page', Value));
end;

function TSkillListParams.Source(const Value: string): TSkillListParams;
begin
  Result := TSkillListParams(Add('source', Value));
end;

{ TSkillList }

destructor TSkillList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TAsynchronousSupport }

procedure TAsynchronousSupport.AsynCreate(const CallBacks: TFunc<TAsynSkill>);
begin
  with TAsynCallBackExec<TAsynSkill, TSkill>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSkill
      begin
        Result := Self.Create;
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynCreate(const SkillId: string;
  const CallBacks: TFunc<TAsynSkill>);
begin
  with TAsynCallBackExec<TAsynSkill, TSkill>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSkill
      begin
        Result := Self.Create(SkillId);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynDelete(const SkillId, Version: string;
  const CallBacks: TFunc<TAsynSkillDeleted>);
begin
  with TAsynCallBackExec<TAsynSkillDeleted, TSkillDeleted>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSkillDeleted
      begin
        Result := Self.Delete(SkillId, Version);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynDelete(const SkillId: string;
  const CallBacks: TFunc<TAsynSkillDeleted>);
begin
  with TAsynCallBackExec<TAsynSkillDeleted, TSkillDeleted>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSkillDeleted
      begin
        Result := Self.Delete(SkillId);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynList(const CallBacks: TFunc<TAsynSkillList>);
begin
  with TAsynCallBackExec<TAsynSkillList, TSkillList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSkillList
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynList(const SkillId: string;
  const CallBacks: TFunc<TAsynSkillList>);
begin
  with TAsynCallBackExec<TAsynSkillList, TSkillList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSkillList
      begin
        Result := Self.List(SkillId);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynList(
  const ParamProc: TProc<TSkillListParams>;
  const CallBacks: TFunc<TAsynSkillList>);
begin
  with TAsynCallBackExec<TAsynSkillList, TSkillList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSkillList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynList(const SkillId: string;
  const ParamProc: TProc<TSkillListParams>;
  const CallBacks: TFunc<TAsynSkillList>);
begin
  with TAsynCallBackExec<TAsynSkillList, TSkillList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSkillList
      begin
        Result := Self.List(SkillId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynRetrieve(const SkillId, Version: string;
  const CallBacks: TFunc<TAsynSkill>);
begin
  with TAsynCallBackExec<TAsynSkill, TSkill>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSkill
      begin
        Result := Self.Retrieve(SkillId, Version);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynRetrieve(const SkillId: string;
  const CallBacks: TFunc<TAsynSkill>);
begin
  with TAsynCallBackExec<TAsynSkill, TSkill>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSkill
      begin
        Result := Self.Retrieve(SkillId);
      end);
  finally
    Free;
  end;
end;

end.
