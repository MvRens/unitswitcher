{$ASSERTIONS ON}
unit UnSwClient;

interface
implementation
uses
  ActnList,
  Classes,
  Dialogs,
  SysUtils,
  ToolsAPI,

  UnSwDialog,
  UnSwObjects;

type
  TUnitSwitcherHook = class(TObject)
  private
    FOldExecute:          TNotifyEvent;
    FViewUnitAction:      TContainedAction;
  protected
    procedure NewExecute(Sender: TObject); virtual;
  public
    constructor Create();
    destructor Destroy(); override;
  end;


{ TUnitSwitcherHook}
constructor TUnitSwitcherHook.Create();
var
  iAction:    Integer;
  ifNTA:      INTAServices;
  pAction:    TContainedAction;

begin
  try
    Assert(Assigned(BorlandIDEServices), 'BorlandIDEServices not available.');
    Assert(Supports(BorlandIDEServices, INTAServices, ifNTA),
                    'BorlandIDEServices does not support the ' +
                    'INTAServices interface.');
    Assert(Supports(BorlandIDEServices, IOTAModuleServices),
                    'BorlandIDEServices does not support the ' +
                    'IOTAModuleServices interface.');

    for iAction := 0 to Pred(ifNTA.ActionList.ActionCount) do
    begin
      pAction := ifNTA.ActionList.Actions[iAction];
      if pAction.Name = 'ViewUnitCommand' then
      begin
        FOldExecute       := pAction.OnExecute;
        pAction.OnExecute := NewExecute;
        FViewUnitAction   := pAction;
        break;
      end;
    end;

    Assert(Assigned(FViewUnitAction), 'ViewUnitCommand action is not' +
                                      'available in the IDE.');
  except
    on E:EAssertionFailed do
      ShowMessage('Error while loading UnitSwitcher: ' + E.Message);
  end;
end;

destructor TUnitSwitcherHook.Destroy();
begin
  if Assigned(FViewUnitAction) then
    FViewUnitAction.OnExecute := FOldExecute;

  inherited;
end;


procedure TUnitSwitcherHook.NewExecute(Sender: TObject);
var
  iModule:    Integer;
  pProject:   IOTAProject;
  pUnits:     TUnSwUnitList;

begin
  pProject  := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
  if not Assigned(pProject) then
    exit;

  pUnits    := TUnSwUnitList.Create();
  try
    for iModule := 0 to Pred(pProject.GetModuleCount) do
      pUnits.Add(TUnSwUnit.Create(pProject.GetModule(iModule)));

    TfrmUnSwDialog.Execute(pUnits);
  finally
    FreeAndNil(pUnits);
  end;
end;


var
  GUnitSwitcher:      TUnitSwitcherHook;

initialization
  GUnitSwitcher := TUnitSwitcherHook.Create();

finalization
  FreeAndNil(GUnitSwitcher);

end.
