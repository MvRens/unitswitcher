// Lowest supported version is Delphi 7. It lacks some OTA functionality
// in comparison to Delphi 2006. Note that UnitSwitcher has not been tested
// on any other Delphi version!
{$UNDEF DELPHI7}
{$IFDEF VER150}
  {$DEFINE DELPHI7}
{$ENDIF}
