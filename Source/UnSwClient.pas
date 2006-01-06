{$ASSERTIONS ON}
unit UnSwClient;

{$I UnSwDefines.inc}

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
    FOldUnitExecute:      TNotifyEvent;
    FOldFormExecute:      TNotifyEvent;
    FViewUnitAction:      TContainedAction;
    FViewFormAction:      TContainedAction;
  protected
    function ActiveFileName(): String;
    {$IFDEF DELPHI7}
    function ActiveGroup(): IOTAProjectGroup;
    {$ENDIF}
    function ActiveProject(): IOTAProject;

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
    {$IFDEF DELPHI7}
    Assert(Supports(BorlandIDEServices, IOTAActionServices),
                    'BorlandIDEServices does not support the ' +
                    'IOTAActionServices interface.');
    {$ENDIF}

    for iAction := 0 to Pred(ifNTA.ActionList.ActionCount) do
    begin
      pAction := ifNTA.ActionList.Actions[iAction];
      if pAction.Name = 'ViewUnitCommand' then
      begin
        FOldUnitExecute   := pAction.OnExecute;
        pAction.OnExecute := NewExecute;
        FViewUnitAction   := pAction;
      end else if pAction.Name = 'ViewFormCommand' then
      begin
        FOldFormExecute   := pAction.OnExecute;
        pAction.OnExecute := NewExecute;
        FViewFormAction   := pAction;
      end;
    end;

    Assert(Assigned(FViewUnitAction), 'ViewUnitCommand action is not' +
                                      'available in the IDE.');
    Assert(Assigned(FViewFormAction), 'ViewFormCommand action is not' +
                                      'available in the IDE.');
  except
    on E:EAssertionFailed do
      ShowMessage('Error while loading UnitSwitcher: ' + E.Message);
  end;
end;

destructor TUnitSwitcherHook.Destroy();
begin
  if Assigned(FViewFormAction) then
    FViewFormAction.OnExecute := FOldFormExecute;

  if Assigned(FViewUnitAction) then
    FViewUnitAction.OnExecute := FOldUnitExecute;

  inherited;
end;


function TUnitSwitcherHook.ActiveFileName(): String;
var
  ifModule:     IOTAModule;

begin
  Result    := '';
  ifModule  := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if Assigned(ifModule) then
  begin
    if Assigned(ifModule.CurrentEditor) then
      Result  := ifModule.FileName;
  end;
end;

{$IFDEF DELPHI7}
function TUnitSwitcherHook.ActiveGroup(): IOTAProjectGroup;
var
  ifModule:     IOTAModule;
  ifModules:    IOTAModuleServices;
  iModule:      Integer;

begin
  Result    := nil;
  ifModules := (BorlandIDEServices as IOTAModuleServices);
  for iModule := 0 to Pred(ifModules.ModuleCount) do
  begin
    ifModule  := ifModules.Modules[iModule];
    if Supports(ifModule, IOTAProjectGroup, Result) then
      break;
  end;
end;
{$ENDIF}

function TUnitSwitcherHook.ActiveProject(): IOTAProject;
{$IFDEF DELPHI7}
var
  ifGroup:      IOTAProjectGroup;
  ifModule:     IOTAModule;
  ifModules:    IOTAModuleServices;
  iModule:      Integer;
{$ENDIF}

begin
  {$IFDEF DELPHI7}
  Result  := nil;
  ifGroup := ActiveGroup();
  if not Assigned(ifGroup) then
  begin
    ifModules := (BorlandIDEServices as IOTAModuleServices);
    for iModule := 0 to Pred(ifModules.ModuleCount) do
    begin
      ifModule  := ifModules.Modules[iModule];
      if Supports(ifModule, IOTAProject, Result) then
        break;
    end;
  end else
    Result  := ifGroup.ActiveProject;
  {$ELSE}
  Result  := (BorlandIDEServices as IOTAModuleServices).GetActiveProject
  {$ENDIF}
end;


procedure TUnitSwitcherHook.NewExecute(Sender: TObject);
var
  iActive:    Integer;
  ifProject:  IOTAProject;
  iModule:    Integer;
  pActive:    TUnSwUnit;
  pUnits:     TUnSwUnitList;

begin
  ifProject := ActiveProject();
  if not Assigned(ifProject) then
    exit;

  pUnits    := TUnSwUnitList.Create();
  try
    pUnits.Add(TUnSwProjectUnit.Create(ifProject));

    for iModule := 0 to Pred(ifProject.GetModuleCount) do
      pUnits.Add(TUnSwModuleUnit.Create(ifProject.GetModule(iModule)));

    pActive := nil;
    iActive := pUnits.IndexOfFileName(ActiveFileName());
    if iActive > -1 then
      pActive := pUnits[iActive];

    pActive := TfrmUnSwDialog.Execute(pUnits, (Sender = FViewFormAction),
                                      pActive);
    if Assigned(pActive) then
      pActive.Activate((Sender = FViewUnitAction));
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
