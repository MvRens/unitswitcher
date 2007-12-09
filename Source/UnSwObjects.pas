{: Implements unit handling.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit UnSwObjects;

{$I BaseSwDefines.inc}

interface
uses
  Classes,
  Contnrs,
  ToolsAPI,

  BaseSwObjects;


type
  // Forward declarations
  TUnSwUnit             = class;
  TUnSwModuleUnit       = class;
  TUnSwProjectUnit      = class;


  IUnSwVisitor          = interface(IBaseSwVisitor)
    ['{A822C25B-5D0F-462F-94DD-47CD6235D79F}']
    procedure VisitModule(const AUnit: TUnSwModuleUnit);
    procedure VisitProject(const AUnit: TUnSwProjectUnit);
  end;


  TUnSwActivateType     = (atForm, atSource, atDFM);

  TUnSwUnit             = class(TBaseSwItem)
  protected
    function GetFileName(): String; virtual;

    procedure OpenModule(const AModule: IOTAModule; const AType: TUnSwActivateType); virtual;
  public
    procedure Activate(const AType: TUnSwActivateType); virtual; abstract;

    property FileName:      String        read GetFileName;
  end;


  TUnSwUnitType         = (
                          swutForm          = 0,
                          swutDataModule    = 1,
                          swutProjUnit      = 2,
                          swutUnit          = 3,
                          swutRc            = 4,
                          swutAsm           = 5,
                          swutDef           = 6,
                          swutObj           = 7,
                          swutRes           = 8,
                          swutLib           = 9,
                          swutTypeLib       = 10,
                          swutPackageImport = 11,
                          swutFormResource  = 12,
                          swutCustom        = 13,
                          swutIDL           = 14
                          );
  TUnSwUnitTypes        = set of TUnSwUnitType;


  TUnSwModuleUnit       = class(TUnSwUnit)
  private
    FModule:        IOTAModuleInfo;
  protected
    function GetFileName(): String; override;
    function GetFormName(): String;
    function GetName(): String; override;
    function GetUnitType(): TUnSwUnitType;
  public
    constructor Create(const AModule: IOTAModuleInfo);
    
    procedure AcceptVisitor(const AVisitor: IBaseSwVisitor); override;

    procedure Activate(const AType: TUnSwActivateType); override;

    property FormName:      String        read GetFormName;
    property UnitType:      TUnSwUnitType read GetUnitType;
  end;


  TUnSwProjectUnit      = class(TUnSwUnit)
  private
    FProject:       IOTAProject;
  protected
    function GetName(): String; override;
    function GetFileName(): String; override;
  public
    constructor Create(const AProject: IOTAProject);
    
    procedure AcceptVisitor(const AVisitor: IBaseSwVisitor); override;

    procedure Activate(const AType: TUnSwActivateType); override;
  end;


  TUnSwUnitList         = class(TBaseSwItemList)
  protected
    function GetItem(Index: Integer): TUnSwUnit;
    procedure SetItem(Index: Integer; Value: TUnSwUnit);
  public
    function IndexOfFileName(const AFileName: String): Integer;

    property Items[Index: Integer]: TUnSwUnit read GetItem
                                              write SetItem; default;
  end;


implementation
uses
  {$IFDEF DELPHI7ORLOWER}
  ActnList,
  {$ENDIF}
  SysUtils;



{ TUnSwUnit }
function TUnSwUnit.GetFileName(): String;
begin
  Result  := '';
end;


procedure TUnSwUnit.OpenModule(const AModule: IOTAModule; const AType: TUnSwActivateType);
var
  editor:         IOTAEditor;
  formEditor:     IOTAFormEditor;
  isForm:         Boolean;
  moduleIndex:    Integer;

begin
  Assert(AType <> atDFM, 'atDFM can not be handled by the OpenModule method');

  formEditor  := nil;
  for moduleIndex := 0 to Pred(AModule.ModuleFileCount) do
  begin
    editor  := AModule.ModuleFileEditors[moduleIndex];
    isForm  := Supports(editor, IOTAFormEditor);

    if (AType = atForm) and isForm and (not Assigned(formEditor)) then
      formEditor  := (editor as IOTAFormEditor);

    if not isForm then
      editor.Show;
  end;

  if Assigned(formEditor) then
    formEditor.Show();
end;


{ TUnSwModuleUnit }
constructor TUnSwModuleUnit.Create(const AModule: IOTAModuleInfo);
begin
  inherited Create();

  FModule := AModule;
end;


procedure TUnSwModuleUnit.Activate(const AType: TUnSwActivateType);
var
  dfmFile:      string;
  ifModule:     IOTAModule;
  handled:      Boolean;

begin
  handled := False;
  
  { Don't use OpenModule for DFM files; can't have a reference to the
    IOTAModule or there'll be errors all over the place. }
  if AType = atDFM then
  begin
    dfmFile := ChangeFileExt(FModule.FileName, '.dfm');
    if FileExists(dfmFile) then
    begin
      (BorlandIDEServices as IOTAActionServices).OpenFile(dfmFile);
      handled := True;
    end;
  end;

  if not handled then
  begin
    ifModule  := FModule.OpenModule();
    if Assigned(ifModule) then
      OpenModule(ifModule, AType);
  end;
end;


procedure TUnSwModuleUnit.AcceptVisitor(const AVisitor: IBaseSwVisitor);
var
  unitVisitor:  IUnSwVisitor;

begin
  if Supports(AVisitor, IUnSwVisitor, unitVisitor) then
    unitVisitor.VisitModule(Self)
  else
    inherited;
end;


function TUnSwModuleUnit.GetName(): String;
begin
  Result  := FModule.Name;
end;


function TUnSwModuleUnit.GetFormName(): String;
begin
  Result  := FModule.FormName;
end;


function TUnSwModuleUnit.GetFileName(): String;
begin
  Result  := FModule.FileName;
end;


function TUnSwModuleUnit.GetUnitType(): TUnSwUnitType;
begin
  Result  := TUnSwUnitType(FModule.ModuleType);

  if Result = swutForm then
    if SameText(FModule.DesignClass, 'TDataModule') then
      Result  := swutDataModule
    else if Length(FModule.FormName) = 0 then
      if Length(FModule.FileName) = 0 then
        Result  := swutProjUnit
      else
        Result  := swutUnit;
end;


{ TUnSwProjectUnit }
constructor TUnSwProjectUnit.Create(const AProject: IOTAProject);
begin
  inherited Create();

  FProject  := AProject;
end;


procedure TUnSwProjectUnit.Activate(const AType: TUnSwActivateType);
{$IFDEF DELPHI7ORLOWER}
var
  actionIndex:    Integer;
  ntaServices:    INTAServices;
  action:         TContainedAction;
{$ENDIF}

begin
  {$IFDEF DELPHI7ORLOWER}
  // Bit of a hack, but opening the file itself will result in Delphi 7
  // reloading the project...
  ntaServices := (BorlandIDEServices as INTAServices);
  for actionIndex := 0 to Pred(ntaServices.ActionList.ActionCount) do
  begin
    action  := ntaServices.ActionList.Actions[actionIndex];
    if action.Name = 'ProjectViewSourceCommand' then
    begin
      action.Execute();
      break;
    end;
  end;
  {$ELSE}
  OpenModule(FProject, atSource);
  {$ENDIF}
end;


procedure TUnSwProjectUnit.AcceptVisitor(const AVisitor: IBaseSwVisitor);
var
  unitVisitor:  IUnSwVisitor;

begin
  if Supports(AVisitor, IUnSwVisitor, unitVisitor) then
    unitVisitor.VisitProject(Self)
  else
    inherited;
end;


function TUnSwProjectUnit.GetName(): String;
begin
  Result  := ChangeFileExt(ExtractFileName(FProject.FileName), '');
end;


function TUnSwProjectUnit.GetFileName(): String;
begin
  Result  := FProject.FileName;
end;


{ TUnSwUnitList}
function TUnSwUnitList.IndexOfFileName(const AFileName: String): Integer;
var
  itemIndex:    Integer;

begin
  Result  := -1;
  if Length(AFileName) = 0 then
    exit;

  for itemIndex := Pred(Count) downto 0 do
    if SameText(Items[itemIndex].FileName, AFileName) then
    begin
      Result  := itemIndex;
      break;
    end;
end;


function TUnSwUnitList.GetItem(Index: Integer): TUnSwUnit;
begin
  Result  := TUnSwUnit(inherited GetItem(Index));
end;


procedure TUnSwUnitList.SetItem(Index: Integer; Value: TUnSwUnit);
begin
  inherited SetItem(Index, Value);
end;

end.
