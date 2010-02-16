unit UnSwReg;

{$I BaseSwDefines.inc}

interface
implementation
uses
  SysUtils,

  CmpSwClient,
  ProjSwClient,
  UnSwClient;

var
  GComponentSwitcher:   TComponentSwitcherHook;
  GUnitSwitcher:        TUnitSwitcherHook;
  GProjectSwitcher:     TProjectSwitcherHook;


initialization
  GComponentSwitcher  := TComponentSwitcherHook.Create;
  GUnitSwitcher       := TUnitSwitcherHook.Create;

  {$IFNDEF DELPHI7ORLOWER}
  GProjectSwitcher    := TProjectSwitcherHook.Create;
  {$ENDIF}


finalization
  FreeAndNil(GProjectSwitcher);
  FreeAndNil(GUnitSwitcher);
  FreeAndNil(GComponentSwitcher);

end.

