unit UnSwReg;

interface
implementation
uses
  SysUtils,

  CmpSwClient,
  UnSwClient;

var
  GComponentSwitcher:   TComponentSwitcherHook;
  GUnitSwitcher:        TUnitSwitcherHook;


initialization
  GComponentSwitcher  := TComponentSwitcherHook.Create();
  GUnitSwitcher       := TUnitSwitcherHook.Create();


finalization
  FreeAndNil(GUnitSwitcher);
  FreeAndNil(GComponentSwitcher);

end.

