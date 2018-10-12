{: Connects UnitSwitcher to the IDE.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit UnSwClient;

{$I BaseSwDefines.inc}

interface
uses
  {$IF CompilerVersion >= 32}
  System.Actions,
  {$IFEND}
  ActnList,
  Classes,
  Dialogs,
  SysUtils,
  ToolsAPI,

  BaseSwClient,
  UnSwDialog,
  UnSwObjects;


type
  TUnitSwitcherHook = class(TBaseSwitcherHook)
  private
    FViewFormAction:    TContainedAction;
    FViewUnitAction:    TContainedAction;
  protected
    function ActiveFileName(): String;
    {$IFDEF DELPHI7ORLOWER}
    function ActiveGroup(): IOTAProjectGroup;
    {$ENDIF}
    function ActiveProject(): IOTAProject;

    procedure NewExecute(Sender: TObject); virtual;
  public
    constructor Create();
  end;


implementation


{ TUnitSwitcherHook}
constructor TUnitSwitcherHook.Create();
begin
  inherited;
  
  try
    {
    Assert(Assigned(BorlandIDEServices), 'BorlandIDEServices not available.');
    Assert(Supports(BorlandIDEServices, INTAServices, ntaServices),
                    'BorlandIDEServices does not support the ' +
                    'INTAServices interface.');
    Assert(Supports(BorlandIDEServices, IOTAModuleServices),
                    'BorlandIDEServices does not support the ' +
                    'IOTAModuleServices interface.');
    Assert(Supports(BorlandIDEServices, IOTAActionServices),
                    'BorlandIDEServices does not support the ' +
                    'IOTAActionServices interface.');
    }

    FViewFormAction := HookIDEAction('ViewFormCommand', NewExecute);
    FViewUnitAction := HookIDEAction('ViewUnitCommand', NewExecute);
  except
    on E:EAssertionFailed do
      ShowMessage('Error while loading UnitSwitcher: ' + E.Message);
  end;
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
  openDFM:        Boolean;
  openType:       TUnSwActivateType;
  fileName:       string;

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
    fileName    := ActiveFileName();

    if SameText(ExtractFileExt(fileName), '.dfm') then
      fileName  := ChangeFileExt(fileName, '.pas');

    activeIndex := unitList.IndexOfFileName(fileName);
    if activeIndex > -1 then
      activeUnit  := unitList[activeIndex];

    selectedUnits := TfrmUnSwDialog.Execute(unitList, (Sender = FViewFormAction),
                                            openDFM, activeUnit);
    if Assigned(selectedUnits) then
    try
      openType := atSource;
      if openDFM then
        openType := atDFM
      else if Sender = FViewFormAction then
        openType := atForm;

      for itemIndex := 0 to Pred(selectedUnits.Count) do
        selectedUnits[itemIndex].Activate(openType);
    finally
      FreeAndNil(selectedUnits);
    end;
  finally
    FreeAndNil(unitList);
  end;
end;

end.
