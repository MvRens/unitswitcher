unit ProjSwObjects;

interface
uses
  ToolsAPI,

  BaseSwObjects;


type
  TProjSwProject = class(TBaseSwItem)
  private
    FProject:         IOTAProject;
    FName:            String;
  protected
    function GetName: String; override;
  public
    constructor Create(AProject: IOTAProject);

    procedure Activate(const AClearSelection: Boolean);
  end;


implementation
uses
  SysUtils, Dialogs;


{ TProjSwProject }
constructor TProjSwProject.Create(AProject: IOTAProject);
begin
  inherited Create;

  FProject  := AProject;
  FName     := ExtractFileName(FProject.ProjectOptions.TargetName);
end;


procedure TProjSwProject.Activate(const AClearSelection: Boolean);
var
  moduleServices: IOTAModuleServices;
  projectGroup:   IOTAProjectGroup;

begin
  moduleServices  := (BorlandIDEServices as IOTAModuleServices);
  projectGroup    := moduleServices.MainProjectGroup;

  if Assigned(projectGroup) then
    projectGroup.ActiveProject  := FProject;
end;


function TProjSwProject.GetName: String;
begin
  Result  := FName;
end;

end.
