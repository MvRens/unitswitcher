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
    procedure VisitItem(const AItem: TBaseSwItem); virtual;
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
    pmnItems:                                   TPopupMenu;
    pmnItemsSelectAll:                          TMenuItem;
    pmnItemsSelectInvert:                       TMenuItem;
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
    procedure lstItemsData(Control: TWinControl; Index: Integer; var Data: string);
    procedure lstItemsDblClick(Sender: TObject);
    procedure lstItemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lstItemsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lstItemsClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FItemList:              TBaseSwItemList;
    FActiveItem:            TBaseSwItem;
    FMRUList:               TStrings;
    FMRUIndex:              Integer;
    FSubFilters:            TStringList;

    FSubFilteredList:       TBaseSwItemList;
    FInputFilteredList:     TBaseSwItemList;

    FSubFilter:             TBaseSwItemSimpleFilter;
    FInputFilter:           TBaseSwItemSimpleFilter;
    FLastFilter:            String;

    FStyleVisitor:          TBaseSwStyleVisitor;
  protected
    function InternalExecute(): TBaseSwItemList; virtual;
    procedure UpdateList(); virtual;

    function CreateItemList(): TBaseSwItemList; virtual;
    function CreateInputFilter(): TBaseSwItemSimpleFilter; virtual;
    function CreateStyleVisitor(): TBaseSwStyleVisitor; virtual;

    function AllowEmptyResult(): Boolean; virtual; abstract;
    function Wildchars(): Boolean; virtual; abstract;
    function ColorsEnabled(): Boolean; virtual;
    
    function GetBaseItemList(): TBaseSwItemList; virtual;
    function GetItemDisplayName(const AItem: TBaseSwItem): String; virtual;
    procedure UpdateItemActions(); virtual;

    function GetActiveItems(): TBaseSwItemList;
    procedure SelectMRUItem();

    function PushFilter(const AFilter: String): Boolean;
    procedure PopFilter();
    procedure UpdateSubFilters();

    procedure LoadSettings(); virtual;
    procedure SaveSettings(); virtual;
    procedure SettingsChanged(); virtual;

    procedure DrawItemText(ACanvas: TCanvas; AItem: TBaseSwItem; ARect: TRect; AState: TOwnerDrawState); virtual;
  protected
    property ActiveItem:  TBaseSwItem     read FActiveItem  write FActiveItem;
    property ItemList:    TBaseSwItemList read FItemList    write FItemList;
    property MRUList:     TStrings        read FMRUList;
  public
    class function Execute(const AItems: TBaseSwItemList; const AActive: TBaseSwItem = nil): TBaseSwItemList;
  end;


implementation
uses
  Messages,
  SysUtils, Dialogs;


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
    ItemList    := AItems;
    ActiveItem  := AActive;
    
    Result      := InternalExecute();
  finally
    Free();
  end;
end;


procedure TfrmBaseSwDialog.FormResize(Sender: TObject);
begin
  lstItems.Invalidate();
end;


procedure TfrmBaseSwDialog.FormShow(Sender: TObject);
begin
  // Setting ListBox.Selected[x] won't work before OnShow...
  UpdateSubFilters();
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
  FSubFilteredList    := CreateItemList();
  FInputFilteredList  := CreateItemList();

  FSubFilter          := CreateInputFilter();
  FInputFilter        := CreateInputFilter();

  FMRUList            := TStringList.Create();
  try
    LoadSettings();
    
    FStyleVisitor := CreateStyleVisitor();
    try
      if Self.ShowModal() = mrOk then
      begin
        if Length(Trim(cmbSearch.Text)) > 0 then
        begin
          iIndex  := MRUList.IndexOf(cmbSearch.Text);
          if iIndex > -1 then
            MRUList.Delete(iIndex);

          while MRUList.Count >= 10 do
            MRUList.Delete(Pred(MRUList.Count));

          mruText := cmbSearch.Text;
          for subFilterIndex := Pred(FSubFilters.Count) downto 0 do
            mruText := FSubFilters[subFilterIndex] + SubFilterSeparator;

          MRUList.Insert(0, mruText);
        end;

        Result  := GetActiveItems();
      end;

      SaveSettings();
    finally
      FreeAndNil(FStyleVisitor);
    end;
  finally
    FreeAndNil(FMRUList);
    FreeAndNil(FInputFilter);
    FreeAndNil(FSubFilter);
    FreeAndNil(FSubFilteredList);
    FreeAndNil(FInputFilteredList);
    FreeAndNil(FSubFilters);
  end;
end;


procedure TfrmBaseSwDialog.LoadSettings();
begin
  cmbSearch.Items.Assign(MRUList);
  SettingsChanged();
end;


procedure TfrmBaseSwDialog.SaveSettings();
begin
end;


procedure TfrmBaseSwDialog.SettingsChanged();
begin
  FInputFilter.Wildchars  := Wildchars;
  FSubFilter.Wildchars    := Wildchars;
end;


procedure TfrmBaseSwDialog.UpdateItemActions();
begin
end;


procedure TfrmBaseSwDialog.UpdateList();
var
  activeUnit:       TBaseSwItem;
  activeUnits:      TBaseSwItemList;
  itemIndex:        Integer;
  listIndex:        Integer;
  filteredList:     TBaseSwItemList;
  selStart:         Integer;

begin
  activeUnits   := GetActiveItems();

  filteredList  := CreateItemList();
  try
    filteredList.Clone(FSubFilteredList);
    FInputFilter.FilterList(filteredList);

    if (filteredList.Count = 0) and (not AllowEmptyResult) then
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
  FSubFilteredList.Clone(GetBaseItemList());

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


function TfrmBaseSwDialog.GetActiveItems(): TBaseSwItemList;
var
  itemIndex:      Integer;

begin
  Result  := nil;

  if Assigned(ActiveItem) then
  begin
    Result              := CreateItemList();
    Result.OwnsObjects  := False;
    Result.Add(ActiveItem);
    ActiveItem          := nil;
  end else if lstItems.SelCount > 0 then
  begin
    Result              := CreateItemList();
    Result.OwnsObjects  := False;
    
    for itemIndex := 0 to Pred(lstItems.Items.Count) do
      if lstItems.Selected[itemIndex] then
        Result.Add(FInputFilteredList[itemIndex]);
  end;
end;


function TfrmBaseSwDialog.GetBaseItemList(): TBaseSwItemList;
begin
  Result  := ItemList;
end;


function TfrmBaseSwDialog.GetItemDisplayName(const AItem: TBaseSwItem): String;
begin
  Result  := AItem.Name;
end;


function TfrmBaseSwDialog.ColorsEnabled(): Boolean;
begin
  Result  := False;
end;


function TfrmBaseSwDialog.CreateItemList(): TBaseSwItemList;
begin
  Result  := TBaseSwItemList.Create();
end;


function TfrmBaseSwDialog.CreateInputFilter(): TBaseSwItemSimpleFilter;
begin
  Result  := TBaseSwItemSimpleNameFilter.Create();
end;


function TfrmBaseSwDialog.CreateStyleVisitor(): TBaseSwStyleVisitor;
begin
  Result  := nil;
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


procedure TfrmBaseSwDialog.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;


procedure TfrmBaseSwDialog.SelectMRUItem();
begin
  cmbSearch.ItemIndex := FMRUIndex;
  ActiveControl       := cmbSearch;
  cmbSearch.SelectAll();

  if Assigned(cmbSearch.OnChange) then
    cmbSearch.OnChange(nil);
end;


procedure TfrmBaseSwDialog.actMRUNextExecute(Sender: TObject);
begin
  if FMRUIndex < Pred(MRUList.Count) then
    Inc(FMRUIndex);

  SelectMRUItem();
end;


procedure TfrmBaseSwDialog.actMRUPriorExecute(Sender: TObject);
begin
  if FMRUIndex >= -1 then
    Dec(FMRUIndex);

  SelectMRUItem();
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


procedure TfrmBaseSwDialog.lstItemsDblClick(Sender: TObject);
begin
  btnOK.Click();
end;


procedure TfrmBaseSwDialog.lstItemsClick(Sender: TObject);
begin
  UpdateItemActions();
end;


procedure TfrmBaseSwDialog.lstItemsData(Control: TWinControl; Index: Integer;
                                      var Data: string);
begin
  Data  := FInputFilteredList[Index].Name;
end;


procedure TfrmBaseSwDialog.DrawItemText(ACanvas: TCanvas; AItem: TBaseSwItem; ARect: TRect; AState: TOwnerDrawState);
var
  text:         String;

begin
  text  := GetItemDisplayName(AItem);
  DrawText(ACanvas.Handle, PChar(text), Length(text), ARect, DT_SINGLELINE or
           DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
end;


procedure TfrmBaseSwDialog.lstItemsDrawItem(Control: TWinControl; Index: Integer;
                                          Rect: TRect; State: TOwnerDrawState);
var
  currentItem:  TBaseSwItem;
  textRect:     TRect;

begin
  with TListBox(Control) do
  begin
    currentItem := FInputFilteredList[Index];
    if Assigned(FStyleVisitor) then
      currentItem.AcceptVisitor(FStyleVisitor);

    if odSelected in State then
    begin
      Canvas.Brush.Color  := clHighlight;
      Canvas.Font.Color   := clHighlightText;
    end else
    begin
      Canvas.Brush.Color  := clWindow;

      if Assigned(FStyleVisitor) and ColorsEnabled() then
        Canvas.Font.Color := FStyleVisitor.Color
      else
        Canvas.Font.Color := clWindowText;
    end;
    Canvas.FillRect(Rect);

    textRect  := Rect;
    InflateRect(textRect, -2, -2);

    if Assigned(FStyleVisitor) then
    begin
      ilsTypes.Draw(Canvas, textRect.Left, textRect.Top, FStyleVisitor.ImageIndex);

      if FStyleVisitor.OverlayIndex > -1 then
        ilsTypes.Draw(Canvas, textRect.Left, textRect.Top, FStyleVisitor.OverlayIndex);
    end;

    Inc(textRect.Left, ilsTypes.Width + 4);
    DrawItemText(Canvas, currentItem, textRect, State);
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
      UpdateItemActions();
    end;
  end;
end;

end.
