{: Contains the UnitSwitcher main dialog.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit UnSwDialog;

{$WARN SYMBOL_PLATFORM OFF}

interface
uses
  ActnList,
  Classes,
  ComCtrls,
  Controls,
  ExtCtrls,
  Forms,
  Graphics,
  ImgList,
  Menus,
  StdCtrls,
  Windows,

  BaseSwDialog,
  BaseSwFilters,
  BaseSwObjects,
  UnSwObjects,
  UnSwFilters;

type
  TUnSwStyleVisitor = class(TBaseSwStyleVisitor, IBaseSwVisitor, IUnSwVisitor)
  protected
    procedure VisitItem(const AItem: TBaseSwItem); override;
    procedure VisitModule(const AUnit: TUnSwModuleUnit); virtual;
    procedure VisitProject(const AUnit: TUnSwProjectUnit); virtual;
  end;


  TfrmUnSwDialog = class(TfrmBaseSwDialog)
    actOpen:                                    TAction;
    actOpenDFM:                                 TAction;
    actOpenDFMProperties:                       TAction;
    actOpenFolder:                              TAction;
    actOpenProperties:                          TAction;
    actReadOnly:                                TAction;
    actSortByName:                              TAction;
    actSortByType:                              TAction;
    chkDataModules:                             TCheckBox;
    chkForms:                                   TCheckBox;
    chkProjectSource:                           TCheckBox;
    chkUnits:                                   TCheckBox;
    pmnItemsOpen:                               TMenuItem;
    pmnItemsOpenDFM:                            TMenuItem;
    pmnItemsOpenDFMProperties:                  TMenuItem;
    pmnItemsOpenFolder:                         TMenuItem;
    pmnItemsOpenProperties:                     TMenuItem;
    pmnItemsReadOnly:                           TMenuItem;
    pmnItemsSep1:                               TMenuItem;
    pmnItemsSep2:                               TMenuItem;
    pmnItemsSep3:                               TMenuItem;
    pmnItemsSep4:                               TMenuItem;
    pmnItemsSortByName:                         TMenuItem;
    pmnItemsSortByType:                         TMenuItem;
    pnlIncludeTypes:                            TPanel;

    procedure actOpenDFMPropertiesExecute(Sender: TObject);
    procedure actOpenFolderExecute(Sender: TObject);
    procedure actOpenPropertiesExecute(Sender: TObject);
    procedure actReadOnlyExecute(Sender: TObject);
    procedure btnConfigurationClick(Sender: TObject);                   
    procedure lstUnitsDblClick(Sender: TObject);
    procedure SortExecute(Sender: TObject);
    procedure TypeFilterChange(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actOpenDFMExecute(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FLoading:               Boolean;
    FFormsOnly:             Boolean;
    FOpenDFM:               Boolean;

    FTypeFilteredList:      TUnSwUnitList;
    FTypeFilter:            TUnSwUnitTypeFilter;
  protected
    function InternalExecute(): TBaseSwItemList; override;
    procedure UpdateTypeFilter(AUpdateList: Boolean);

    function CreateItemList(): TBaseSwItemList; override;
    function CreateInputFilter(): TBaseSwItemSimpleFilter; override;
    function CreateStyleVisitor(): TBaseSwStyleVisitor; override;

    function AllowEmptyResult(): Boolean; override;
    function ColorsEnabled(): Boolean; override;

    function GetBaseItemList(): TBaseSwItemList; override;
    function GetItemDisplayName(const AItem: TBaseSwItem): String; override;

    procedure UpdateItemActions(); override;

    function GetActiveUnits(): TUnSwUnitList;

    procedure LoadSettings(); override;
    procedure SaveSettings(); override;
  public
    class function Execute(const AUnits: TUnSwUnitList;
                           const AFormsOnly: Boolean;
                           out AOpenDFM: Boolean;
                           const AActive: TUnSwUnit = nil): TUnSwUnitList;
  end;

  
implementation
uses
  Messages,
  ShellAPI,
  SysUtils,

  UnSwConfiguration,
  UnSwSettings;


type
  TUnSwOpenVisitor = class(TInterfacedPersistent, IBaseSwVisitor, IUnSwVisitor)
  private
    FProcessed:     TStringList;
  protected
    function IsProcessed(const AFileName: String; const ARegister: Boolean = True): Boolean;
    procedure OpenFile(const AFileName: String); virtual; abstract;

    procedure VisitItem(const AItem: TBaseSwItem); virtual;
    procedure VisitModule(const AUnit: TUnSwModuleUnit); virtual;
    procedure VisitProject(const AUnit: TUnSwProjectUnit); virtual;
  public
    constructor Create();
    destructor Destroy(); override;
  end;


  TUnSwOpenFolderVisitor = class(TUnSwOpenVisitor)
  protected
    procedure OpenFile(const AFileName: String); override;
  end;


  TUnSwOpenPropertiesVisitor = class(TUnSwOpenVisitor)
  protected
    procedure OpenFile(const AFileName: String); override;
  end;


  TUnSwOpenDFMPropertiesVisitor = class(TUnSwOpenPropertiesVisitor)
  protected
    procedure VisitModule(const AUnit: TUnSwModuleUnit); override;
    procedure VisitProject(const AUnit: TUnSwProjectUnit); override;
  end;


  TUnSwReadOnlyVisitor = class(TUnSwOpenVisitor)
  private
    FReadOnlyCount: Integer;
  protected
    procedure OpenFile(const AFileName: String); override;
  public
    property ReadOnlyCount:     Integer read FReadOnlyCount;
  end;


  TUnSwSetReadOnlyVisitor = class(TUnSwOpenVisitor)
  private
    FReadOnlyFlag: Boolean;
  protected
    procedure OpenFile(const AFileName: String); override;
  public
    property ReadOnlyFlag: Boolean read FReadOnlyFlag write FReadOnlyFlag;
  end;


const
  SubFilterSeparator  = ' '#187' ';


{$R *.dfm}


function IsReadOnly(const AFileName: String): Boolean;
var
  iAttr:    Integer;
  
begin
  iAttr   := FileGetAttr(AFileName);
  Result  := (iAttr <> -1) and ((iAttr and faReadOnly) <> 0);
end;


{ TUnSwOpenVisitor }
constructor TUnSwOpenVisitor.Create();
begin
  inherited Create();

  FProcessed                := TStringList.Create();
  FProcessed.CaseSensitive  := False;
end;


destructor TUnSwOpenVisitor.Destroy();
begin
  FreeAndNil(FProcessed);

  inherited;
end;


function TUnSwOpenVisitor.IsProcessed(const AFileName: String;
                                      const ARegister: Boolean): Boolean;
begin
  Result  := True;
  if FileExists(AFileName) or DirectoryExists(AFileName) then
  begin
    Result  := (FProcessed.IndexOf(AFileName) > -1);
    if (not Result) and ARegister then
      FProcessed.Add(AFileName);
  end;
end;


procedure TUnSwOpenVisitor.VisitItem(const AItem: TBaseSwItem);
begin
end;


procedure TUnSwOpenVisitor.VisitModule(const AUnit: TUnSwModuleUnit);
begin
  OpenFile(AUnit.FileName);
end;


procedure TUnSwOpenVisitor.VisitProject(const AUnit: TUnSwProjectUnit);
begin
  OpenFile(AUnit.FileName);
end;


{ TUnSwOpenFolderVisitor }
procedure TUnSwOpenFolderVisitor.OpenFile(const AFileName: String);
var
  sFile:        String;
  sPath:        String;
  sParams:      String;

begin
  sFile := ExpandFileName(AFileName);
  sPath := ExtractFilePath(sFile);
  if not IsProcessed(sPath) then
  begin
    sParams := '/e,';

    // If it's a file, have explorer highlight it
    if not DirectoryExists(sFile) then
      sParams := sParams + '/select,';

    sParams := sParams + ExtractShortPathName(sFile);
    ShellExecute(0, 'open', 'explorer.exe', PChar(sParams), nil, SW_SHOWNORMAL);
  end;
end;


{ TUnSwOpenPropertiesVisitor }
procedure TUnSwOpenPropertiesVisitor.OpenFile(const AFileName: String);
var
  pInfo:        TShellExecuteInfo;

begin
  if not IsProcessed(AFileName) then
  begin
    // Regular ShellExecute doesn't work
    FillChar(pInfo, SizeOf(pInfo), #0);
    pInfo.cbSize  := SizeOf(TShellExecuteInfo);
    pInfo.lpFile  := PChar(AFileName);
    pInfo.nShow   := SW_SHOWNORMAL;
    pInfo.fMask   := SEE_MASK_INVOKEIDLIST;
    pInfo.lpVerb  := 'properties';
    ShellExecuteEx(@pInfo);
  end;
end;


{ TUnSwOpenDFMPropertiesVisitor }
procedure TUnSwOpenDFMPropertiesVisitor.VisitModule(const AUnit: TUnSwModuleUnit);
begin
  OpenFile(ChangeFileExt(AUnit.FileName, '.dfm'));
end;


procedure TUnSwOpenDFMPropertiesVisitor.VisitProject(const AUnit: TUnSwProjectUnit);
begin
end;


{ TUnSwReadOnlyVisitor }
procedure TUnSwReadOnlyVisitor.OpenFile(const AFileName: String);
begin
  if not IsProcessed(AFileName) then
    if IsReadOnly(AFileName) then
      Inc(FReadOnlyCount);
end;


{ TUnSwSetReadOnlyVisitor }
procedure TUnSwSetReadOnlyVisitor.OpenFile(const AFileName: String);
var
  fileInfo: TSearchRec;
  fileAttr: Integer;
  path: String;

begin
  if not IsProcessed(AFileName) then
  begin
    path  := ExtractFilePath(AFileName);
    if FindFirst(ChangeFileExt(AFileName, '.*'), faAnyFile, fileInfo) = 0 then
    begin
      repeat
        fileAttr := FileGetAttr(path + fileInfo.Name);
        if fileAttr <> -1 then
        begin
          if ReadOnlyFlag then
            fileAttr  := fileAttr or faReadOnly
          else
            fileAttr  := fileAttr and not faReadOnly;

          FileSetAttr(path + fileInfo.Name, fileAttr);
        end;
      until FindNext(fileInfo) <> 0;

      FindClose(fileInfo);
    end;
  end;
end;


{ TUnSwStyleVisitor }
procedure TUnSwStyleVisitor.VisitItem(const AItem: TBaseSwItem);
begin
  if (AItem is TUnSwUnit) and IsReadOnly(TUnSwUnit(AItem).FileName) then
    OverlayIndex  := 5
  else
    OverlayIndex  := -1;
end;


procedure TUnSwStyleVisitor.VisitModule(const AUnit: TUnSwModuleUnit);
begin
  VisitItem(AUnit);
  case AUnit.UnitType of
    swutUnit:
      begin
        Color       := Settings.Colors.Units;
        ImageIndex  := 1;
      end;
    swutForm:
      begin
        Color       := Settings.Colors.Forms;
        ImageIndex  := 2;
      end;
    swutDataModule:
      begin
        Color       := Settings.Colors.DataModules;
        ImageIndex  := 3;
      end;
  else
    Color       := clWindowText;
    ImageIndex  := 0;
  end;
end;


procedure TUnSwStyleVisitor.VisitProject(const AUnit: TUnSwProjectUnit);
begin
  VisitItem(AUnit);
  Color       := Settings.Colors.ProjectSource;
  ImageIndex  := 4;
end;


{ TfrmUnSwDialog }
class function TfrmUnSwDialog.Execute(const AUnits: TUnSwUnitList;
                                      const AFormsOnly: Boolean;
                                      out AOpenDFM: Boolean;
                                      const AActive: TUnSwUnit): TUnSwUnitList;
begin
  with Self.Create(nil) do
  try
    ItemList    := AUnits;
    ActiveItem  := AActive;
    FFormsOnly  := AFormsOnly;

    Result      := TUnSwUnitList(InternalExecute());
    AOpenDFM    := FOpenDFM;
  finally
    Free();
  end;
end;


procedure TfrmUnSwDialog.FormShow(Sender: TObject);
begin
  UpdateTypeFilter(False);
  inherited;
end;


function TfrmUnSwDialog.GetActiveUnits(): TUnSwUnitList;
begin
  Result  := (GetActiveItems() as TUnSwUnitList);
end;


function TfrmUnSwDialog.GetBaseItemList(): TBaseSwItemList;
begin
  Result  := FTypeFilteredList;
end;


function TfrmUnSwDialog.GetItemDisplayName(const AItem: TBaseSwItem): String;
begin
  if FFormsOnly and (AItem is TUnSwModuleUnit) then
    Result  := TUnSwModuleUnit(AItem).FormName
  else
    Result  := inherited GetItemDisplayName(AItem);
end;


function TfrmUnSwDialog.InternalExecute(): TBaseSwItemList;
begin
  FTypeFilteredList   := TUnSwUnitList.Create();
  FTypeFilter         := TUnSwUnitTypeFilter.Create;
  try
    if FFormsOnly then
    begin
      chkProjectSource.Visible  := False;
      chkUnits.Visible          := False;
      Self.Caption              := 'UnitSwitcher - View Form';
    end else
      Self.Caption              := 'UnitSwitcher - View Unit';

    Result  := inherited InternalExecute();
  finally
    FreeAndNil(FTypeFilter);
    FreeAndNil(FTypeFilteredList);
  end;
end;


procedure TfrmUnSwDialog.UpdateItemActions();
var
  bDFM:       Boolean;
  bUnits:     Boolean;
  iUnit:      Integer;
  pUnits:     TUnSwUnitList;
  pVisitor:   TUnSwReadOnlyVisitor;
  sStatus:    String;

begin
  { Read-only status }
  pUnits  := GetActiveUnits();
  if Assigned(pUnits) then
  try
    pVisitor  := TUnSwReadOnlyVisitor.Create();
    try
      pUnits.AcceptVisitor(pVisitor);
      actReadOnly.Checked := (pVisitor.ReadOnlyCount > 0);

      sStatus := '';
      if pVisitor.ReadOnlyCount > 0 then
        if pVisitor.ReadOnlyCount = 1 then
          sStatus := '1 read-only unit selected'
        else
          sStatus := Format('%d read-only units selected',
                            [pVisitor.ReadOnlyCount]);

      sbStatus.Panels[0].Text := sStatus;
    finally
      FreeAndNil(pVisitor);
    end;
  finally
    FreeAndNil(pUnits);
  end;

  { Properties }
  bDFM      := False;
  bUnits    := False;

  pUnits    := GetActiveUnits();
  if Assigned(pUnits) then
  try
    bUnits  := (pUnits.Count > 0);

    for iUnit := 0 to Pred(pUnits.Count) do
      if (pUnits[iUnit] is TUnSwModuleUnit) and
         (TUnSwModuleUnit(pUnits[iUnit]).UnitType in [swutForm, swutDataModule]) then
      begin
        bDFM  := True;
        break;
      end;
  finally
    FreeAndNil(pUnits);
  end;

  actOpenFolder.Enabled         := bUnits;
  actOpenProperties.Enabled     := bUnits;
  actOpenDFMProperties.Enabled  := bDFM;
end;


function SortByName(Item1, Item2: Pointer): Integer;
begin
  Result  := CompareText(TUnSwUnit(Item1).Name, TUnSwUnit(Item2).Name)
end;


function SortByType(Item1, Item2: Pointer): Integer;
const
  Above = -1;
  Equal = 0;
  Below = 1;

  function SortByModuleType(Item1, Item2: TUnSwUnitType): Integer;
  begin
    Result  := Equal;
    if Item1 <> Item2 then
      case Item1 of
        swutForm:
          case Item2 of
            swutDataModule:   Result  := Below;
            swutUnit:         Result  := Above;
          end;
        swutDataModule:       Result  := Above;
        swutUnit:             Result  := Below;
      end;
  end;

var
  pItem1:     TUnSwUnit;
  pItem2:     TUnSwUnit;

begin
  // #ToDo3 Refactor SortByType

  // The following order is assumed:
  //    Project source, DataModules, Forms, Units
  Result  := Equal;
  pItem1  := TUnSwUnit(Item1);
  pItem2  := TUnSwUnit(Item2);

  if pItem1.ClassType <> pItem2.ClassType then
  begin
    if pItem1 is TUnSwProjectUnit then
      Result  := Above
    else if pItem2 is TUnSwProjectUnit then
      Result  := Below;
  end else if pItem1 is TUnSwModuleUnit then
    Result  := SortByModuleType(TUnSwModuleUnit(pItem1).UnitType,
                                TUnSwModuleUnit(pItem2).UnitType);

  if Result = Equal then
    Result  := SortByName(Item1, Item2);
end;


procedure TfrmUnSwDialog.UpdateTypeFilter(AUpdateList: Boolean);
begin
  FTypeFilter.IncludeUnits          := ((not FFormsOnly) and chkUnits.Checked);
  FTypeFilter.IncludeProjectSource  := ((not FFormsOnly) and chkProjectSource.Checked);
  FTypeFilter.IncludeForms          := chkForms.Checked;
  FTypeFilter.IncludeDataModules    := chkDataModules.Checked;

  FTypeFilteredList.Clone(ItemList);
  FTypeFilter.FilterList(FTypeFilteredList);

  if actSortByName.Checked then
    FTypeFilteredList.Sort(SortByName)
  else
    FTypeFilteredList.Sort(SortByType);

  if AUpdateList then
    UpdateSubFilters();
end;


procedure TfrmUnSwDialog.LoadSettings();
var
  dialogSettings:       TUnSwDialogSettings;

begin
  if FFormsOnly then
    dialogSettings  := Settings.FormsDialog
  else
    dialogSettings  := Settings.UnitsDialog;

  FLoading  := True;
  try
    chkDataModules.Checked    := dialogSettings.IncludeDataModules;
    chkForms.Checked          := dialogSettings.IncludeForms;
    chkUnits.Checked          := dialogSettings.IncludeUnits;
    chkProjectSource.Checked  := dialogSettings.IncludeProjectSource;

    case dialogSettings.Sort of
      dsName: actSortByName.Checked := True;
      dsType: actSortByType.Checked := True;
    end;

    MRUList.Assign(dialogSettings.MRUList);
    
    Self.ClientWidth          := dialogSettings.Width;
    Self.ClientHeight         := dialogSettings.Height;

    inherited LoadSettings();
  finally
    FLoading  := False;
  end;
end;


procedure TfrmUnSwDialog.SaveSettings();
var
  dialogSettings:       TUnSwDialogSettings;

begin
  if FFormsOnly then
    dialogSettings  := Settings.FormsDialog
  else
    dialogSettings  := Settings.UnitsDialog;

  dialogSettings.IncludeDataModules   := chkDataModules.Checked;
  dialogSettings.IncludeForms         := chkForms.Checked;
  dialogSettings.IncludeUnits         := chkUnits.Checked;
  dialogSettings.IncludeProjectSource := chkProjectSource.Checked;

  if actSortByName.Checked then
    dialogSettings.Sort               := dsName
  else
    dialogSettings.Sort               := dsType;

  dialogSettings.Width                := Self.ClientWidth;
  dialogSettings.Height               := Self.ClientHeight;

  dialogSettings.MRUList.Assign(MRUList);

  Settings.Save();

  inherited SaveSettings();
end;


procedure TfrmUnSwDialog.SortExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := True;
  UpdateTypeFilter(True);
end;


procedure TfrmUnSwDialog.actOpenFolderExecute(Sender: TObject);
var
  pUnits:     TUnSwUnitList;

begin
  pUnits  := GetActiveUnits();
  if Assigned(pUnits) then
  try
    pUnits.AcceptVisitor(TUnSwOpenFolderVisitor.Create());
  finally
    FreeAndNil(pUnits);
  end;
end;


procedure TfrmUnSwDialog.actOpenPropertiesExecute(Sender: TObject);
var
  pUnits:     TUnSwUnitList;

begin
  pUnits  := GetActiveUnits();
  if Assigned(pUnits) then
  try
    pUnits.AcceptVisitor(TUnSwOpenPropertiesVisitor.Create());
  finally
    FreeAndNil(pUnits);
  end;
end;


procedure TfrmUnSwDialog.actOpenDFMPropertiesExecute(Sender: TObject);
var
  pUnits:     TUnSwUnitList;

begin
  pUnits  := GetActiveUnits();
  if Assigned(pUnits) then
  try
    pUnits.AcceptVisitor(TUnSwOpenDFMPropertiesVisitor.Create());
  finally
    FreeAndNil(pUnits);
  end;
end;


procedure TfrmUnSwDialog.btnConfigurationClick(Sender: TObject);
begin
  if TfrmUnSwConfiguration.Execute() then
    lstItems.Invalidate();
end;


function TfrmUnSwDialog.CreateItemList(): TBaseSwItemList;
begin
  Result  := TUnSwUnitList.Create();
end;


function TfrmUnSwDialog.ColorsEnabled(): Boolean;
begin
  Result  := Settings.Colors.Enabled;
end;


function TfrmUnSwDialog.CreateStyleVisitor(): TBaseSwStyleVisitor;
begin
  Result  := TUnSwStyleVisitor.Create();
end;


function TfrmUnSwDialog.CreateInputFilter(): TBaseSwItemSimpleFilter;
begin
  if FFormsOnly then
    Result  := TUnSwUnitSimpleFormNameFilter.Create()
  else
    Result  := TBaseSwItemSimpleNameFilter.Create();
end;


procedure TfrmUnSwDialog.TypeFilterChange(Sender: TObject);
begin
  if not FLoading then
    UpdateTypeFilter(True);
end;


procedure TfrmUnSwDialog.lstUnitsDblClick(Sender: TObject);
begin
  btnOK.Click();
end;


procedure TfrmUnSwDialog.actReadOnlyExecute(Sender: TObject);
var
  pUnits:       TUnSwUnitList;
  pVisitor:     TUnSwSetReadOnlyVisitor;

begin
  pUnits  := GetActiveUnits();
  if Assigned(pUnits) then
  try
    pVisitor  := TUnSwSetReadOnlyVisitor.Create();
    try
      pVisitor.ReadOnlyFlag := not actReadOnly.Checked;
      pUnits.AcceptVisitor(pVisitor);
    finally
      FreeAndNil(pVisitor);
    end;
  finally
    FreeAndNil(pUnits);
    
    lstItems.Invalidate();
    UpdateItemActions();
  end;
end;


function TfrmUnSwDialog.AllowEmptyResult(): Boolean;
begin
  Result  := Settings.Filter.AllowEmptyResult;
end;


procedure TfrmUnSwDialog.actOpenExecute(Sender: TObject);
begin
  FOpenDFM := False;
  ModalResult := mrOk;
end;


procedure TfrmUnSwDialog.actOpenDFMExecute(Sender: TObject);
begin
  FOpenDFM := True;
  ModalResult := mrOk;
end;

procedure TfrmUnSwDialog.btnOKClick(Sender: TObject);
begin
  FOpenDFM := ((GetKeyState(VK_SHIFT) and 128) <> 0);
  
  inherited;
end;

end.
