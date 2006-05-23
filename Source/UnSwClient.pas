{: Connects UnitSwitcher to the IDE.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
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
    {$IFDEF DELPHI7ORLOWER}
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
  actionIndex:    Integer;
  ntaServices:    INTAServices;
  action:         TContainedAction;

begin
  try
    Assert(Assigned(BorlandIDEServices), 'BorlandIDEServices not available.');
    Assert(Supports(BorlandIDEServices, INTAServices, ntaServices),
                    'BorlandIDEServices does not support the ' +
                    'INTAServices interface.');
    Assert(Supports(BorlandIDEServices, IOTAModuleServices),
                    'BorlandIDEServices does not support the ' +
                    'IOTAModuleServices interface.');
    {$IFDEF DELPHI7ORLOWER}
    Assert(Supports(BorlandIDEServices, IOTAActionServices),
                    'BorlandIDEServices does not support the ' +
                    'IOTAActionServices interface.');
    {$ENDIF}

    for actionIndex := 0 to Pred(ntaServices.ActionList.ActionCount) do
    begin
      action  := ntaServices.ActionList.Actions[actionIndex];
      if action.Name = 'ViewUnitCommand' then
      begin
        FOldUnitExecute   := action.OnExecute;
        action.OnExecute  := NewExecute;
        FViewUnitAction   := action;
      end else if action.Name = 'ViewFormCommand' then
      begin
        FOldFormExecute   := action.OnExecute;
        action.OnExecute  := NewExecute;
        FViewFormAction   := action;
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
  module:     IOTAModule;

begin
  Result  := '';
  module  := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if Assigned(module) then
  begin
    if Assigned(module.CurrentEditor) then
      Result  := module.FileName;
  end;
end;

{$IFDEF DELPHI7ORLOWER}
function TUnitSwitcherHook.ActiveGroup(): IOTAProjectGroup;
var
  module:           IOTAModule;
  moduleServices:   IOTAModuleServices;
  moduleIndex:      Integer;

begin
  Result          := nil;
  moduleServices  := (BorlandIDEServices as IOTAModuleServices);
  for moduleIndex := 0 to Pred(moduleServices.ModuleCount) do
  begin
    module  := moduleServices.Modules[moduleIndex];
    if Supports(module, IOTAProjectGroup, Result) then
      break;
  end;
end;
{$ENDIF}

function TUnitSwitcherHook.ActiveProject(): IOTAProject;
{$IFDEF DELPHI7ORLOWER}
var
  projectGroup:       IOTAProjectGroup;
  module:             IOTAModule;
  moduleServices:     IOTAModuleServices;
  moduleIndex:        Integer;
{$ENDIF}

begin
  {$IFDEF DELPHI7ORLOWER}
  Result        := nil;
  projectGroup  := ActiveGroup();
  if not Assigned(projectGroup) then
  begin
    moduleServices  := (BorlandIDEServices as IOTAModuleServices);
    for moduleIndex := 0 to Pred(moduleServices.ModuleCount) do
    begin
      module  := moduleServices.Modules[moduleIndex];
      if Supports(module, IOTAProject, Result) then
        break;
    end;
  end else
    Result  := projectGroup.ActiveProject;
  {$ELSE}
  Result  := (BorlandIDEServices as IOTAModuleServices).GetActiveProject();
  {$ENDIF}
end;


procedure TUnitSwitcherHook.NewExecute(Sender: TObject);
var
  activeIndex:    Integer;
  activeUnit:     TUnSwUnit;
  itemIndex:      Integer;
  moduleIndex:    Integer;
  project:        IOTAProject;
  selectedUnits:  TUnSwUnitList;
  unitList:       TUnSwUnitList;

begin
  project := ActiveProject();
  if not Assigned(project) then
    exit;

  unitList  := TUnSwUnitList.Create();
  try
    unitList.Add(TUnSwProjectUnit.Create(project));

    for moduleIndex := 0 to Pred(project.GetModuleCount) do
      unitList.Add(TUnSwModuleUnit.Create(project.GetModule(moduleIndex)));

    activeUnit  := nil;
    activeIndex := unitList.IndexOfFileName(ActiveFileName());
    if activeIndex > -1 then
      activeUnit  := unitList[activeIndex];

    selectedUnits := TfrmUnSwDialog.Execute(unitList, (Sender = FViewFormAction),
                                            activeUnit);
    if Assigned(selectedUnits) then
    try
      for itemIndex := 0 to Pred(selectedUnits.Count) do
        selectedUnits[itemIndex].Activate((Sender = FViewUnitAction));
    finally
      FreeAndNil(selectedUnits);
    end;
  finally
    FreeAndNil(unitList);
  end;
end;


var
  GUnitSwitcher:      TUnitSwitcherHook;

initialization
  GUnitSwitcher := TUnitSwitcherHook.Create();

finalization
  FreeAndNil(GUnitSwitcher);

end.
