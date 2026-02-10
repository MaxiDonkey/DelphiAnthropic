unit Anthropic.API.Consts;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

const
  /// <summary>
  /// Allowlist of JSON field names whose values must be intercepted and temporarily shielded before the REST layer,
  /// then restored afterward.
  /// </summary>
  /// <remarks>
  /// These fields commonly carry nested JSON objects/arrays (polymorphic payloads) or metadata blocks.
  /// <para>
  /// • When such values pass through a REST interceptor, they may be re-escaped, normalized, or otherwise
  /// altered (notably around quotes and backslashes), which can corrupt embedded structures.
  /// </para>
  /// <para>
  /// • Shielding (see <see cref="Anthropic.API.JSONShield.TJsonPolyShield.Prepare"/>) replaces sensitive characters inside
  /// the targeted object/array payload with private-use markers and wraps the whole block as a string, preserving the
  /// exact original content through transport/interception.
  /// </para>
  /// <para>
  /// • Restoration is performed by
  /// <see cref="Anthropic.API.JSONShield.TJsonPolyUnshield.Restore"/>, which re-expands the markers and rebuilds the
  /// original object/array shape.
  /// </para>
  /// </remarks>
  PROTECTED_FIELD: TArray<string> = [
    'input'
  ];

implementation

end.
