{: Connects ComponentSwitcher to the IDE.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit CmpSwClient;

interface
uses
  ActnList,
  Classes,
  Dialogs,
  SysUtils,
  ToolsAPI,

  BaseSwClient;

  
type
  TComponentSwitcherHook = class(TBaseSwitcherHook)
  protected
    function ActiveModule(): IOTAModule;
    function ActiveEditor(): IOTAEditor;

//    function ActiveFileName(): String;
//    {$IFDEF DELPHI7ORLOWER}
//    function ActiveGroup(): IOTAProjectGroup;
//    {$ENDIF}
//    function ActiveProject(): IOTAProject;

    procedure NewExecute(Sender: TObject); virtual;
  public
    constructor Create();
  end;


implementation


{ TComponentSwitcherHook}
constructor TComponentSwitcherHook.Create();
begin
  inherited;

  try
    HookIDEAction('SearchFindCommand', NewExecute);
  except
    on E:EAssertionFailed do
      ShowMessage('Error while loading ComponentSwitcher: ' + E.Message);
  end;
end;


(*
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
*)



function TComponentSwitcherHook.ActiveModule(): IOTAModule;
begin
  Result := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
end;


function TComponentSwitcherHook.ActiveEditor(): IOTAEditor;
var
  module:   IOTAModule;

begin
  Result  := nil;
  module  := ActiveModule();

  if Assigned(module) then
    Result  := activeModule.CurrentEditor;
end;


procedure TComponentSwitcherHook.NewExecute(Sender: TObject);
var
  editor:     IOTAEditor;
  formEditor: IOTAFormEditor;
  name:       String;

begin
  editor  := ActiveEditor();
  if Supports(editor, IOTAFormEditor, formEditor) then
  begin
    name := '';
    formEditor.GetRootComponent.GetPropValueByName('Name', name);
    ShowMessage(name);
  end else
    OldActionExecute(Sender);
end;

end.
