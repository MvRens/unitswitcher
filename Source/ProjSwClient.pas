{: Connects ProjectSwitcher to the IDE.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit ProjSwClient;


interface
uses
  ActnList,
  Classes,
  Dialogs,
  SysUtils,
  ToolsAPI,

  BaseSwClient;


type
  TProjectSwitcherHook = class(TBaseSwitcherHook)
  private
    FSwitchAction: TAction;
  protected
    procedure SwitchExecute(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation
uses
  Menus,
  Windows,

  BaseSwObjects,
  ProjSwDialog,
  ProjSwObjects;


{ TProjectSwitcherHook}
constructor TProjectSwitcherHook.Create;
var
  actionList: TCustomActionList;

begin
  inherited;

  actionList    := GetIDEActionList;
  FSwitchAction := TAction.Create(actionList);

  FSwitchAction.Caption     := 'ProjectSwitcher';
  FSwitchAction.ShortCut    := ShortCut(VK_F12, [ssCtrl, ssAlt, ssShift]);
  FSwitchAction.SecondaryShortCuts.Add(ShortCutToText(ShortCut(VK_F11, [ssCtrl, ssAlt, ssShift])));
  FSwitchAction.OnExecute   := SwitchExecute;
  FSwitchAction.ActionList  := actionList;
end;


destructor TProjectSwitcherHook.Destroy;
begin
  FreeAndNil(FSwitchAction);

  inherited;
end;


function SortByName(Item1, Item2: Pointer): Integer;
begin
  Result  := CompareText(TProjSwProject(Item1).Name, TProjSwProject(Item2).Name)
end;


procedure TProjectSwitcherHook.SwitchExecute(Sender: TObject);
var
  projectList:    TBaseSwItemList;
  activeItem:     TProjSwProject;
  projectItem:    TProjSwProject;
  moduleServices: IOTAModuleServices;
  project:        IOTAProject;
  activeProject:  IOTAProject;
  projectIndex:   Integer;
  selectedItems:  TBaseSwItemList;

begin
  projectList := TBaseSwItemList.Create;
  try
    activeItem      := nil;
    moduleServices  := (BorlandIDEServices as IOTAModuleServices);

    if not Assigned(moduleServices.MainProjectGroup) then
      Exit;

    activeProject := moduleServices.GetActiveProject;

    for projectIndex := Pred(moduleServices.MainProjectGroup.ProjectCount) downto 0 do
    begin
      project     := moduleServices.MainProjectGroup.Projects[projectIndex];
      projectItem := TProjSwProject.Create(project);

      if project = activeProject then
        activeItem := projectItem;

      projectList.Add(projectItem);
    end;

    if projectList.Count > 0 then
    begin
      projectList.Sort(SortByName);

      selectedItems := TfrmProjSwDialog.Execute(projectList, activeItem);
      if Assigned(selectedItems) then
      try
        if selectedItems.Count > 0 then
        begin
          projectItem := TProjSwProject(selectedItems[0]);
          projectItem.Activate(True);
        end;
      finally
        FreeAndNil(selectedItems);
      end;
    end;
  finally
    FreeAndNil(projectList);
  end;
end;

end.
