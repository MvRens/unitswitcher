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

  UnSwObjects,
  UnSwFilters;

type
  TUnSwStyleVisitor = class(TUnSwNoRefIntfObject, IUnSwVisitor)
  private
    FColor:             TColor;
    FImageIndex:        Integer;
    FOverlayIndex:      Integer;
  protected
    procedure VisitUnit(const AUnit: TUnSwUnit);
    procedure VisitModule(const AUnit: TUnSwModuleUnit);
    procedure VisitProject(const AUnit: TUnSwProjectUnit);
  public
    property Color:           TColor  read FColor;
    property ImageIndex:      Integer read FImageIndex;
    property OverlayIndex:    Integer read FOverlayIndex;
  end;

  TfrmUnSwDialog = class(TForm)
    actMRUNext:                                 TAction;
    actMRUPrior:                                TAction;
    actOpenDFMProperties:                       TAction;
    actOpenFolder:                              TAction;
    actOpenProperties:                          TAction;
    actReadOnly:                                TAction;
    actSelectAll:                               TAction;
    actSelectInvert:                            TAction;
    actSortByName:                              TAction;
    actSortByType:                              TAction;
    alMain:                                     TActionList;
    btnCancel:                                  TButton;
    btnConfiguration:                           TButton;
    btnOK:                                      TButton;
    chkDataModules:                             TCheckBox;
    chkForms:                                   TCheckBox;
    chkProjectSource:                           TCheckBox;
    chkUnits:                                   TCheckBox;
    cmbSearch:                                  TComboBox;
    ilsTypes:                                   TImageList;
    lblSubFilters:                              TLabel;
    lstUnits:                                   TListBox;
    pmnUnits:                                   TPopupMenu;
    pmnUnitsOpenDFMProperties:                  TMenuItem;
    pmnUnitsOpenFolder:                         TMenuItem;
    pmnUnitsOpenProperties:                     TMenuItem;
    pmnUnitsReadOnly:                           TMenuItem;
    pmnUnitsSelectAll:                          TMenuItem;
    pmnUnitsSelectInvert:                       TMenuItem;
    pmnUnitsSep1:                               TMenuItem;
    pmnUnitsSep2:                               TMenuItem;
    pmnUnitsSep3:                               TMenuItem;
    pmnUnitsSortByName:                         TMenuItem;
    pmnUnitsSortByType:                         TMenuItem;
    pnlButtons:                                 TPanel;
    pnlIncludeTypes:                            TPanel;
    pnlMain:                                    TPanel;
    pnlSearch:                                  TPanel;
    pnlSubFilters:                              TPanel;
    sbStatus:                                   TStatusBar;

    procedure actMRUNextExecute(Sender: TObject);
    procedure actMRUPriorExecute(Sender: TObject);
    procedure actOpenDFMPropertiesExecute(Sender: TObject);
    procedure actOpenFolderExecute(Sender: TObject);
    procedure actOpenPropertiesExecute(Sender: TObject);
    procedure actReadOnlyExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectInvertExecute(Sender: TObject);
    procedure btnConfigurationClick(Sender: TObject);
    procedure cmbSearchChange(Sender: TObject);
    procedure cmbSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cmbSearchKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstUnitsClick(Sender: TObject);
    procedure lstUnitsData(Control: TWinControl; Index: Integer; var Data: string);
    procedure lstUnitsDblClick(Sender: TObject);
    procedure lstUnitsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lstUnitsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SortExecute(Sender: TObject);
    procedure TypeFilterChange(Sender: TObject);
  private
    FLoading:               Boolean;
    FUnitList:              TUnSwUnitList;
    FActiveUnit:            TUnSwUnit;
    FFormsOnly:             Boolean;
    FMRUList:               TStrings;
    FMRUIndex:              Integer;
    FSubFilters:            TStringList;

    FTypeFilteredList:      TUnSwUnitList;
    FSubFilteredList:       TUnSwUnitList;
    FInputFilteredList:     TUnSwUnitList;

    FTypeFilter:            TUnSwUnitTypeFilter;
    FSubFilter:             TUnSwUnitSimpleFilter;
    FInputFilter:           TUnSwUnitSimpleFilter;
    FLastFilter:            String;

    FStyleVisitor:          TUnSwStyleVisitor;

    function InternalExecute(): TUnSwUnitList;
    procedure UpdateTypeFilter();
    procedure UpdateList();

    function GetActiveUnits(): TUnSwUnitList;
    procedure SelectMRUItem();

    function PushFilter(const AFilter: String): Boolean;
    procedure PopFilter();
    procedure UpdateSubFilters();

    procedure LoadSettings();
    procedure SaveSettings();

    procedure UpdateUnitActions();
  public
    class function Execute(const AUnits: TUnSwUnitList;
                           const AFormsOnly: Boolean;
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
  TUnSwOpenVisitor = class(TInterfacedObject, IUnSwVisitor)
  private
    FProcessed:     TStringList;
  protected
    function IsProcessed(const AFileName: String; const ARegister: Boolean = True): Boolean;
    procedure OpenFile(const AFileName: String); virtual; abstract;

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
procedure TUnSwStyleVisitor.VisitUnit(const AUnit: TUnSwUnit);
begin
  if IsReadOnly(AUnit.FileName) then
    FOverlayIndex := 5
  else
    FOverlayIndex := -1;
end;


procedure TUnSwStyleVisitor.VisitModule(const AUnit: TUnSwModuleUnit);
begin
  VisitUnit(AUnit);
  case AUnit.UnitType of
    swutUnit:
      begin
        FColor      := Settings.Colors.Units;
        FImageIndex := 1;
      end;
    swutForm:
      begin
        FColor      := Settings.Colors.Forms;
        FImageIndex := 2;
      end;
    swutDataModule:
      begin
        FColor      := Settings.Colors.DataModules;
        FImageIndex := 3;
      end
  else
    FColor      := clWindowText;
    FImageIndex := 0;
  end;
end;


procedure TUnSwStyleVisitor.VisitProject(const AUnit: TUnSwProjectUnit);
begin
  VisitUnit(AUnit);
  FColor        := Settings.Colors.ProjectSource;
  FImageIndex   := 4;
end;


{ TfrmUnSwDialog }
class function TfrmUnSwDialog.Execute(const AUnits: TUnSwUnitList;
                                      const AFormsOnly: Boolean;
                                      const AActive: TUnSwUnit): TUnSwUnitList;
begin
  with Self.Create(nil) do
  try
    FUnitList   := AUnits;
    FActiveUnit := AActive;
    FFormsOnly  := AFormsOnly;
    Result      := InternalExecute();
  finally
    Free();
  end;
end;


procedure TfrmUnSwDialog.FormResize(Sender: TObject);
begin
  lstUnits.Invalidate();
end;


procedure TfrmUnSwDialog.FormShow(Sender: TObject);
begin
  // Setting ListBox.Selected[x] won't work before OnShow...
  UpdateTypeFilter();
end;


function TfrmUnSwDialog.InternalExecute(): TUnSwUnitList;
type
  TUnSwUnitSimpleFilterClass  = class of TUnSwUnitSimpleFilter;

var
  iIndex:       Integer;
  pClass:       TUnSwUnitSimpleFilterClass;

begin
  Result              := nil;
  FSubFilters         := TStringList.Create();
  FTypeFilteredList   := TUnSwUnitList.Create();
  FSubFilteredList    := TUnSwUnitList.Create();
  FInputFilteredList  := TUnSwUnitList.Create();
  FTypeFilter         := TUnSwUnitTypeFilter.Create;

  if FFormsOnly then
    pClass            := TUnSwUnitSimpleFormNameFilter
  else
    pClass            := TUnSwUnitSimpleNameFilter;

  FSubFilter          := pClass.Create;
  FInputFilter        := pClass.Create;

  try
    LoadSettings();

    if FFormsOnly then
    begin
      chkProjectSource.Visible  := False;
      chkUnits.Visible          := False;
      Self.Caption              := 'UnitSwitcher - View Form';
    end else
      Self.Caption              := 'UnitSwitcher - View Unit';

    FStyleVisitor := TUnSwStyleVisitor.Create();
    try
      if Self.ShowModal() = mrOk then
      begin
        if Length(Trim(cmbSearch.Text)) > 0 then
        begin
          iIndex  := FMRUList.IndexOf(cmbSearch.Text);
          if iIndex > -1 then
            FMRUList.Delete(iIndex);

          while FMRUList.Count >= 10 do
            FMRUList.Delete(Pred(FMRUList.Count));

          FMRUList.Insert(0, cmbSearch.Text);
        end;
        
        Result  := GetActiveUnits();
      end;

      SaveSettings();
    finally
      FreeAndNil(FStyleVisitor);
    end;
  finally
    FreeAndNil(FInputFilter);
    FreeAndNil(FSubFilter); 
    FreeAndNil(FTypeFilter);
    FreeAndNil(FSubFilteredList);
    FreeAndNil(FInputFilteredList);
    FreeAndNil(FTypeFilteredList);
    FreeAndNil(FSubFilters);
  end;
end;


procedure TfrmUnSwDialog.UpdateUnitActions();
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


procedure TfrmUnSwDialog.UpdateList();
var
  activeUnit:       TUnSwUnit;
  activeUnits:      TUnSwUnitList;
  itemIndex:        Integer;
  listIndex:        Integer;
  filteredList:     TUnSwUnitList;
  selStart:         Integer;

begin
  activeUnits   := GetActiveUnits();

  filteredList  := TUnSwUnitList.Create();
  try
    filteredList.Clone(FSubFilteredList);
    FInputFilter.FilterList(filteredList);

    if (filteredList.Count = 0) and (not Settings.Filter.AllowEmptyResult) then
    begin
      { Only enforce AllowEmptyResult when adding to the filter }
      if Length(FInputFilter.Filter) > Length(FLastFilter) then
      begin
        FInputFilter.Filter := FLastFilter;
        selStart            := cmbSearch.SelStart;
        cmbSearch.Text      := FLastFilter;
        cmbSearch.SelStart  := selStart;
        Exit;
      end;
    end;

    FLastFilter     := FInputFilter.Filter;
    FInputFilteredList.Clone(filteredList);
  finally
    FreeAndNil(filteredList);
  end;


  lstUnits.Count  := FInputFilteredList.Count;
  if FInputFilteredList.Count > 0 then
  begin
    lstUnits.ClearSelection();

    if Assigned(activeUnits) then
    try
      for itemIndex := 0 to Pred(activeUnits.Count) do
      begin
        activeUnit  := activeUnits[itemIndex];
        listIndex   := FInputFilteredList.IndexOf(activeUnit);
        if listIndex > -1 then
          lstUnits.Selected[listIndex]  := True;
      end;
    finally
      FreeAndNil(activeUnits);
    end;

    if lstUnits.SelCount = 0 then
      lstUnits.Selected[0]  := True;
  end;

  if Assigned(lstUnits.OnClick) then
    lstUnits.OnClick(nil);
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


procedure TfrmUnSwDialog.UpdateTypeFilter();
begin
  FTypeFilter.IncludeUnits          := ((not FFormsOnly) and chkUnits.Checked);
  FTypeFilter.IncludeProjectSource  := ((not FFormsOnly) and chkProjectSource.Checked);
  FTypeFilter.IncludeForms          := chkForms.Checked;
  FTypeFilter.IncludeDataModules    := chkDataModules.Checked;

  FTypeFilteredList.Clone(FUnitList);
  FTypeFilter.FilterList(FTypeFilteredList);

  if actSortByName.Checked then
    FTypeFilteredList.Sort(SortByName)
  else
    FTypeFilteredList.Sort(SortByType);

  UpdateSubFilters();
end;


procedure TfrmUnSwDialog.PopFilter();
begin
  if FSubFilters.Count > 0 then
  begin
    FSubFilters.Delete(Pred(FSubFilters.Count));
    UpdateSubFilters();
  end;
end;


procedure TfrmUnSwDialog.UpdateSubFilters();
var
  iFilter:        Integer;
  sFilters:       String;

begin
  FSubFilteredList.Clone(FTypeFilteredList);

  if FSubFilters.Count > 0 then
  begin
    for iFilter := 0 to Pred(FSubFilters.Count) do
    begin
      sFilters          := sFilters + FSubFilters[iFilter] + ' '#187' ';
      FSubFilter.Filter := FSubFilters[iFilter];
      FSubFilter.FilterList(FSubFilteredList);
    end;

    lblSubFilters.Caption := Trim(sFilters);
    pnlSubFilters.Visible := True;
  end else
    pnlSubFilters.Visible := False;

  UpdateList();
end;


function TfrmUnSwDialog.PushFilter(const AFilter: String): Boolean;
var
  sFilter:      String;

begin
  sFilter := Trim(AFilter);  
  Result  := (Length(sFilter) > 0) and (FSubFilters.IndexOf(AFilter) = -1);
  if Result then
  begin
    FSubFilters.Add(AFilter);
    UpdateSubFilters();
  end;
end;


function TfrmUnSwDialog.GetActiveUnits(): TUnSwUnitList;
var
  itemIndex:      Integer;

begin
  Result  := nil;

  if Assigned(FActiveUnit) then
  begin
    Result              := TUnSwUnitList.Create();
    Result.OwnsObjects  := False;
    Result.Add(FActiveUnit);
    FActiveUnit         := nil;
  end else if lstUnits.SelCount > 0 then
  begin
    Result              := TUnSwUnitList.Create();
    Result.OwnsObjects  := False;
    for itemIndex := 0 to Pred(lstUnits.Items.Count) do
      if lstUnits.Selected[itemIndex] then
        Result.Add(FInputFilteredList[itemIndex]);
  end;
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

    FMRUList                  := dialogSettings.MRUList;
    cmbSearch.Items.Assign(FMRUList);

    Self.ClientWidth          := dialogSettings.Width;
    Self.ClientHeight         := dialogSettings.Height;
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

  Settings.Save();
end;


procedure TfrmUnSwDialog.actSelectAllExecute(Sender: TObject);
begin
  lstUnits.SelectAll();
end;


procedure TfrmUnSwDialog.actSelectInvertExecute(Sender: TObject);
var
  iItem:      Integer;

begin
  for iItem := Pred(lstUnits.Count) downto 0 do
    lstUnits.Selected[iItem]  := not lstUnits.Selected[iItem];
end;


procedure TfrmUnSwDialog.SortExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := True;
  UpdateTypeFilter();
end;


procedure TfrmUnSwDialog.SelectMRUItem();
begin
  cmbSearch.ItemIndex := FMRUIndex;
  ActiveControl       := cmbSearch;
  cmbSearch.SelectAll();

  if Assigned(cmbSearch.OnChange) then
    cmbSearch.OnChange(nil);
end;


procedure TfrmUnSwDialog.actMRUNextExecute(Sender: TObject);
begin
  if FMRUIndex < Pred(FMRUList.Count) then
    Inc(FMRUIndex);

  SelectMRUItem();
end;


procedure TfrmUnSwDialog.actMRUPriorExecute(Sender: TObject);
begin
  if FMRUIndex >= -1 then
    Dec(FMRUIndex);

  SelectMRUItem();
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
    lstUnits.Invalidate();
end;


procedure TfrmUnSwDialog.cmbSearchChange(Sender: TObject);
begin
  if cmbSearch.Text <> FInputFilter.Filter then
  begin
    FInputFilter.Filter := cmbSearch.Text;
    UpdateList();
  end;
end;


procedure TfrmUnSwDialog.cmbSearchKeyDown(Sender: TObject; var Key: Word;
                                          Shift: TShiftState);
begin
  if not cmbSearch.DroppedDown then
    if ((Shift = []) and (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT])) or
       ((Shift = [ssCtrl]) and (Key in [VK_HOME, VK_END])) or
       ((Shift = [ssShift]) and (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT])) then
    begin
      lstUnits.Perform(WM_KEYDOWN, Key, 0);
      Key := 0;
    end else if Shift = [ssCtrl] then
      case Key of
        VK_TAB:
          begin
            if PushFilter(cmbSearch.Text) then
              cmbSearch.Text  := '';

            Key := 0;
          end;
        VK_BACK:
          begin
            cmbSearch.Text      := '';
            FInputFilter.Filter := '';
            PopFilter();
            Key := 0;
          end;
      end;
end;


procedure TfrmUnSwDialog.cmbSearchKeyPress(Sender: TObject; var Key: Char);
begin
  // Ctrl-Backspace
  if Key = #127 then
    Key := #0;
end;


procedure TfrmUnSwDialog.TypeFilterChange(Sender: TObject);
begin
  if not FLoading then
    UpdateTypeFilter();
end;


procedure TfrmUnSwDialog.lstUnitsDblClick(Sender: TObject);
begin
  btnOK.Click();
end;


procedure TfrmUnSwDialog.lstUnitsClick(Sender: TObject);
begin
  UpdateUnitActions();
end;


procedure TfrmUnSwDialog.lstUnitsData(Control: TWinControl; Index: Integer;
                                      var Data: string);
begin
  Data  := FInputFilteredList[Index].Name;
end;


procedure TfrmUnSwDialog.lstUnitsDrawItem(Control: TWinControl; Index: Integer;
                                          Rect: TRect; State: TOwnerDrawState);
var
  currentUnit:  TUnSwUnit;
  textRect:     TRect;
  text:         String;

begin
  with TListBox(Control) do
  begin
    currentUnit := FInputFilteredList[Index];
    currentUnit.AcceptVisitor(FStyleVisitor);
    
    if FFormsOnly and (currentUnit is TUnSwModuleUnit) then
      text  := TUnSwModuleUnit(currentUnit).FormName
    else
      text  := currentUnit.Name;

    if odSelected in State then
    begin
      Canvas.Brush.Color  := clHighlight;
      Canvas.Font.Color   := clHighlightText;
    end else
    begin
      Canvas.Brush.Color  := clWindow;
      if Settings.Colors.Enabled then
        Canvas.Font.Color := FStyleVisitor.Color
      else
        Canvas.Font.Color := clWindowText;
    end;
    Canvas.FillRect(Rect);

    textRect  := Rect;
    InflateRect(textRect, -2, -2);
    ilsTypes.Draw(Canvas, textRect.Left, textRect.Top, FStyleVisitor.ImageIndex);

    if FStyleVisitor.OverlayIndex > -1 then
      ilsTypes.Draw(Canvas, textRect.Left, textRect.Top, FStyleVisitor.OverlayIndex);

    Inc(textRect.Left, ilsTypes.Width + 4);
    DrawText(Canvas.Handle, PChar(text), Length(text), textRect, DT_SINGLELINE or
             DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
  end;
end;


procedure TfrmUnSwDialog.lstUnitsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  itemIndex: Integer;

begin
  { Bij rechtermuisknop het item selecteren indien deze niet al
    geselecteerd was }
  if Button = mbRight then
  begin
    itemIndex := lstUnits.ItemAtPos(Point(X, Y), True);
    if (itemIndex > -1) and (not lstUnits.Selected[itemIndex]) then
    begin
      lstUnits.ClearSelection;
      lstUnits.Selected[itemIndex]  := True;
      UpdateUnitActions();
    end;
  end;
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
    
    lstUnits.Invalidate();
    UpdateUnitActions();
  end;
end;

end.
