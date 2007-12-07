{: Contains the base Switcher dialog.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit BaseSwDialog;

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

  BaseSwFilters,
  BaseSwObjects;

  
type
  TBaseSwStyleVisitor = class(TInterfacedPersistent, IBaseSwVisitor)
  private
    FColor:             TColor;
    FImageIndex:        Integer;
    FOverlayIndex:      Integer;
  protected
    procedure VisitItem(const AItem: TBaseSwItem);
  public
    property Color:           TColor  read FColor         write FColor;
    property ImageIndex:      Integer read FImageIndex    write FImageIndex;
    property OverlayIndex:    Integer read FOverlayIndex  write FOverlayIndex;
  end;


  TfrmBaseSwDialog = class(TForm)
    actMRUNext:                                 TAction;
    actMRUPrior:                                TAction;
    actSelectAll:                               TAction;
    actSelectInvert:                            TAction;
    alMain:                                     TActionList;
    btnCancel:                                  TButton;
    btnOK:                                      TButton;
    cmbSearch:                                  TComboBox;
    ilsTypes:                                   TImageList;
    lblSubFilters:                              TLabel;
    lstItems:                                   TListBox;
    pmnUnits:                                   TPopupMenu;
    pmnUnitsOpen:                               TMenuItem;
    pmnUnitsOpenDFM:                            TMenuItem;
    pmnUnitsOpenDFMProperties:                  TMenuItem;
    pmnUnitsOpenFolder:                         TMenuItem;
    pmnUnitsOpenProperties:                     TMenuItem;
    pmnUnitsReadOnly:                           TMenuItem;
    pmnUnitsSelectAll:                          TMenuItem;
    pmnUnitsSelectInvert:                       TMenuItem;
    pmnUnitsSep1:                               TMenuItem;
    pmnUnitsSep2:                               TMenuItem;
    pmnUnitsSep3:                               TMenuItem;
    pmnUnitsSep4:                               TMenuItem;
    pmnUnitsSortByName:                         TMenuItem;
    pmnUnitsSortByType:                         TMenuItem;
    pnlButtons:                                 TPanel;
    pnlMain:                                    TPanel;
    pnlSearch:                                  TPanel;
    pnlSubFilters:                              TPanel;
    sbStatus:                                   TStatusBar;

    procedure actMRUNextExecute(Sender: TObject);
    procedure actMRUPriorExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectInvertExecute(Sender: TObject);
    procedure cmbSearchChange(Sender: TObject);
    procedure cmbSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cmbSearchKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstItemsClick(Sender: TObject);
    procedure lstItemsData(Control: TWinControl; Index: Integer; var Data: string);
    procedure lstItemsDblClick(Sender: TObject);
    procedure lstItemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lstItemsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnOKClick(Sender: TObject);
  private
    FLoading:               Boolean;
    FItemList:              TBaseSwItemList;
    FActiveItem:            TBaseSwItem;
    FFormsOnly:             Boolean;
    FMRUList:               TStrings;
    FMRUIndex:              Integer;
    FSubFilters:            TStringList;
    FOpenDFM:               Boolean;

//    FTypeFilteredList:      TUnSwUnitList;
    FSubFilteredList:       TBaseSwItemList;
    FInputFilteredList:     TBaseSwItemList;

//    FTypeFilter:            TUnSwUnitTypeFilter;
    FSubFilter:             TBaseSwItemSimpleFilter;
    FInputFilter:           TBaseSwItemSimpleFilter;
    FLastFilter:            String;

    FStyleVisitor:          TBaseSwStyleVisitor;
  protected
    function InternalExecute(): TBaseSwItemList; virtual;
//    procedure UpdateTypeFilter();
    procedure UpdateList(); virtual;

    function GetActiveItems(): TBaseSwItemList;
    procedure SelectMRUItem();

    function PushFilter(const AFilter: String): Boolean;
    procedure PopFilter();
    procedure UpdateSubFilters();

    procedure LoadSettings(); virtual;
    procedure SaveSettings(); virtual;

    procedure UpdateUnitActions();
  public
    class function Execute(const AItems: TBaseSwItemList; const AActive: TBaseSwItem = nil): TBaseSwItemList;
  end;

  
implementation
uses
  SysUtils;


const
  SubFilterSeparator  = ' '#187' ';


{$R *.dfm}


{ TBaseSwStyleVisitor }
procedure TBaseSwStyleVisitor.VisitItem(const AItem: TBaseSwItem);
begin
  Color         := clDefault;
  ImageIndex    := -1;
  OverlayIndex  := -1;
end;


{ TfrmUnSwDialog }
class function TfrmBaseSwDialog.Execute(const AItems: TBaseSwItemList;
                                        const AActive: TBaseSwItem): TBaseSwItemList;
begin
  with Self.Create(nil) do
  try
    FItemList   := AItems;
    FActiveItem := AActive;
    
    Result      := InternalExecute();
  finally
    Free();
  end;
end;


procedure TfrmBaseSwDialog.FormResize(Sender: TObject);
begin
  lstItems.Invalidate();
end;


function TfrmBaseSwDialog.InternalExecute(): TBaseSwItemList;
type
  TBaseSwItemSimpleFilterClass  = class of TBaseSwItemSimpleFilter;

var
  iIndex:           Integer;
  mruText:          String;
  subFilterIndex:   Integer;

begin
  Result              := nil;
  FSubFilters         := TStringList.Create();
//  FTypeFilteredList   := TUnSwUnitList.Create();
  FSubFilteredList    := TBaseSwItemList.Create();
  FInputFilteredList  := TBaseSwItemList.Create();
//  FTypeFilter         := TUnSwUnitTypeFilter.Create;

  try
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

          mruText := cmbSearch.Text;
          for subFilterIndex := Pred(FSubFilters.Count) downto 0 do
            mruText := FSubFilters[subFilterIndex] + SubFilterSeparator;

          FMRUList.Insert(0, mruText);
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
//    FreeAndNil(FTypeFilter);
    FreeAndNil(FSubFilteredList);
    FreeAndNil(FInputFilteredList);
//    FreeAndNil(FTypeFilteredList);
    FreeAndNil(FSubFilters);
  end;
end;


procedure TfrmBaseSwDialog.UpdateUnitActions();
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


procedure TfrmBaseSwDialog.UpdateList();
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


  lstItems.Count  := FInputFilteredList.Count;
  if FInputFilteredList.Count > 0 then
  begin
    lstItems.ClearSelection();

    if Assigned(activeUnits) then
    try
      for itemIndex := 0 to Pred(activeUnits.Count) do
      begin
        activeUnit  := activeUnits[itemIndex];
        listIndex   := FInputFilteredList.IndexOf(activeUnit);
        if listIndex > -1 then
          lstItems.Selected[listIndex]  := True;
      end;
    finally
      FreeAndNil(activeUnits);
    end;

    if lstItems.SelCount = 0 then
      lstItems.Selected[0]  := True;
  end;

  if Assigned(lstItems.OnClick) then
    lstItems.OnClick(nil);
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


procedure TfrmBaseSwDialog.UpdateTypeFilter();
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


procedure TfrmBaseSwDialog.PopFilter();
begin
  if FSubFilters.Count > 0 then
  begin
    FSubFilters.Delete(Pred(FSubFilters.Count));
    UpdateSubFilters();
  end;
end;


procedure TfrmBaseSwDialog.UpdateSubFilters();
var
  iFilter:        Integer;
  sFilters:       String;

begin
  FSubFilteredList.Clone(FTypeFilteredList);

  if FSubFilters.Count > 0 then
  begin
    for iFilter := 0 to Pred(FSubFilters.Count) do
    begin
      sFilters          := sFilters + FSubFilters[iFilter] + SubFilterSeparator;
      FSubFilter.Filter := FSubFilters[iFilter];
      FSubFilter.FilterList(FSubFilteredList);
    end;

    lblSubFilters.Caption := Trim(sFilters);
    pnlSubFilters.Visible := True;
  end else
    pnlSubFilters.Visible := False;

  UpdateList();
end;


function TfrmBaseSwDialog.PushFilter(const AFilter: String): Boolean;
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


function TfrmBaseSwDialog.GetActiveUnits(): TUnSwUnitList;
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
  end else if lstItems.SelCount > 0 then
  begin
    Result              := TUnSwUnitList.Create();
    Result.OwnsObjects  := False;
    
    for itemIndex := 0 to Pred(lstItems.Items.Count) do
      if lstItems.Selected[itemIndex] then
        Result.Add(FInputFilteredList[itemIndex]);
  end;
end;


procedure TfrmBaseSwDialog.LoadSettings();
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


procedure TfrmBaseSwDialog.SaveSettings();
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


procedure TfrmBaseSwDialog.actSelectAllExecute(Sender: TObject);
begin
  lstItems.SelectAll();
end;


procedure TfrmBaseSwDialog.actSelectInvertExecute(Sender: TObject);
var
  iItem:      Integer;

begin
  for iItem := Pred(lstItems.Count) downto 0 do
    lstItems.Selected[iItem]  := not lstItems.Selected[iItem];
end;


procedure TfrmBaseSwDialog.SortExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := True;
  UpdateTypeFilter();
end;


procedure TfrmBaseSwDialog.SelectMRUItem();
var
  mruText:      String;

begin
  mruText := FMRUList[FMRUIndex];
  cmbSearch.ItemIndex := FMRUIndex;
  ActiveControl       := cmbSearch;
  cmbSearch.SelectAll();

  if Assigned(cmbSearch.OnChange) then
    cmbSearch.OnChange(nil);
end;


procedure TfrmBaseSwDialog.actMRUNextExecute(Sender: TObject);
begin
  if FMRUIndex < Pred(FMRUList.Count) then
    Inc(FMRUIndex);

  SelectMRUItem();
end;


procedure TfrmBaseSwDialog.actMRUPriorExecute(Sender: TObject);
begin
  if FMRUIndex >= -1 then
    Dec(FMRUIndex);

  SelectMRUItem();
end;


procedure TfrmBaseSwDialog.actOpenFolderExecute(Sender: TObject);
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


procedure TfrmBaseSwDialog.actOpenPropertiesExecute(Sender: TObject);
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


procedure TfrmBaseSwDialog.actOpenDFMPropertiesExecute(Sender: TObject);
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


procedure TfrmBaseSwDialog.btnConfigurationClick(Sender: TObject);
begin
  if TfrmUnSwConfiguration.Execute() then
    lstItems.Invalidate();
end;


procedure TfrmBaseSwDialog.cmbSearchChange(Sender: TObject);
begin
  if cmbSearch.Text <> FInputFilter.Filter then
  begin
    FInputFilter.Filter := cmbSearch.Text;
    UpdateList();
  end;
end;


procedure TfrmBaseSwDialog.cmbSearchKeyDown(Sender: TObject; var Key: Word;
                                          Shift: TShiftState);
begin
  if not cmbSearch.DroppedDown then
    if ((Shift = []) and (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT])) or
       ((Shift = [ssCtrl]) and (Key in [VK_HOME, VK_END])) or
       ((Shift = [ssShift]) and (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT])) then
    begin
      lstItems.Perform(WM_KEYDOWN, Key, 0);
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


procedure TfrmBaseSwDialog.cmbSearchKeyPress(Sender: TObject; var Key: Char);
begin
  // Ctrl-Backspace
  if Key = #127 then
    Key := #0;
end;


procedure TfrmBaseSwDialog.TypeFilterChange(Sender: TObject);
begin
  if not FLoading then
    UpdateTypeFilter();
end;


procedure TfrmBaseSwDialog.lstItemsDblClick(Sender: TObject);
begin
  btnOK.Click();
end;


procedure TfrmBaseSwDialog.lstItemsClick(Sender: TObject);
begin
  UpdateUnitActions();
end;


procedure TfrmBaseSwDialog.lstItemsData(Control: TWinControl; Index: Integer;
                                      var Data: string);
begin
  Data  := FInputFilteredList[Index].Name;
end;


procedure TfrmBaseSwDialog.lstItemsDrawItem(Control: TWinControl; Index: Integer;
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


procedure TfrmBaseSwDialog.lstItemsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  itemIndex: Integer;

begin
  { Bij rechtermuisknop het item selecteren indien deze niet al
    geselecteerd was }
  if Button = mbRight then
  begin
    itemIndex := lstItems.ItemAtPos(Point(X, Y), True);
    if (itemIndex > -1) and (not lstItems.Selected[itemIndex]) then
    begin
      lstItems.ClearSelection;
      lstItems.Selected[itemIndex]  := True;
      UpdateUnitActions();
    end;
  end;
end;


procedure TfrmBaseSwDialog.actReadOnlyExecute(Sender: TObject);
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
    UpdateUnitActions();
  end;
end;


procedure TfrmBaseSwDialog.actOpenExecute(Sender: TObject);
begin
  FOpenDFM := False;
  ModalResult := mrOk;
end;


procedure TfrmBaseSwDialog.actOpenDFMExecute(Sender: TObject);
begin
  FOpenDFM := True;
  ModalResult := mrOk;
end;

procedure TfrmBaseSwDialog.btnOKClick(Sender: TObject);
begin
  FOpenDFM := ((GetKeyState(VK_SHIFT) and 128) <> 0);
  ModalResult := mrOk;
end;

end.
