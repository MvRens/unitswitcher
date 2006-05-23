// Lowest supprted version is Delphi 6. It lacks some OTA functionality
// in comparison to Delphi 2006. Note that UnitSwitcher has not been tested
// on any other Delphi version!
{$UNDEF DELPHI6}
{$UNDEF DELPHI7}
{$UNDEF DELPHI7ORLOWER}

{$IFDEF VER140}
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI7ORLOWER}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI7ORLOWER}
{$ENDIF}
