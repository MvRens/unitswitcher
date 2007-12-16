unit CmpSwDialog;

interface
uses
  ActnList,
  Classes,
  ComCtrls,
  Contnrs,
  Controls,
  ExtCtrls,
  Graphics,
  ImgList,
  IniFiles,
  Menus,
  StdCtrls,
  Windows,

  BaseSwDialog,
  BaseSwObjects,
  CmpSwFilters, UnSwDialog;


type
  TCmpSwStyleVisitor  = class(TBaseSwStyleVisitor)
  private
    FImageList: TImageList;
    FImageMap: TStringHash;
  protected
    procedure VisitItem(const AItem: TBaseSwItem); override;
                                                    
    function GetComponentPackage(const AClassName: String): String;
    function LoadComponentImage(const APackageName, AClassName: String): Integer;
    procedure ResizeBitmap(const ABitmap: Graphics.TBitmap; const AWidth, AHeight: Integer);
  public
    constructor Create(AImageList: TImageList);
    destructor Destroy(); override;
  end;


  TfrmCmpSwDialog = class(TfrmBaseSwDialog)
    pmnItemsSep1: TMenuItem;
    pmnItemsFilters: TMenuItem;
    pmnItemsFilterSelected: TMenuItem;
    actFilterSelected: TAction;
    pnlFilters: TPanel;
    gbFilters: TGroupBox;
    btnMoreFilters: TButton;
    pmnMoreFilters: TPopupMenu;
    actSortByName: TAction;
    actSortByType: TAction;
    pmnItemsSortByName: TMenuItem;
    pmnItemsSortByType: TMenuItem;
    pmnItemsSep2: TMenuItem;
    btnConfiguration: TButton;

    procedure btnMoreFiltersClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SortExecute(Sender: TObject);
    procedure btnConfigurationClick(Sender: TObject);
  private
    FClassFilteredList:   TBaseSwItemList;
    FClassFilter:         TCmpSwComponentClassFilter;

    FFilterCheckBoxes:    TObjectList;
    FOtherGroup:          TCmpSwFilterGroup;
  protected
    function InternalExecute(): TBaseSwItemList; override;

    function CreateStyleVisitor(): TBaseSwStyleVisitor; override;
    function GetBaseItemList(): TBaseSwItemList; override;

    function AllowEmptyResult(): Boolean; override;
    function ColorsEnabled(): Boolean; override;

    procedure LoadSettings(); override;
    procedure SaveSettings(); override;

    procedure DrawItemText(ACanvas: TCanvas; AItem: TBaseSwItem; ARect: TRect); override;
    procedure UpdateClassFilter();
    procedure SortList();

    procedure BuildFilterCheckboxes();
    function CreateFilterMenuItem(AParent: TMenuItem; AGroup: TCmpSwFilterGroup; AItemIndex: Integer): TMenuItem;

    procedure FilterCheckBoxClick(Sender: TObject);
    procedure FilterMenuItemClick(Sender: TObject);

    property ClassFilter:         TCmpSwComponentClassFilter  read FClassFilter;
    property ClassFilteredList:   TBaseSwItemList             read FClassFilteredList;
  end;


implementation
uses
  SysUtils,
  ToolsAPI,

  CmpSwConfiguration,
  CmpSwObjects,
  CmpSwSettings;


{$R *.dfm}


{ TCmpSwStyleVisitor }
constructor TCmpSwStyleVisitor.Create(AImageList: TImageList);
begin
  inherited Create();

  FImageList  := AImageList;
  FImageMap   := TStringHash.Create();
end;


destructor TCmpSwStyleVisitor.Destroy();
begin
  FreeAndNil(FImageMap);

  inherited;
end;


procedure TCmpSwStyleVisitor.VisitItem(const AItem: TBaseSwItem);
var
  component:  TCmpSwComponent;
  package:    String;

begin
  inherited;

  component   := (AItem as TCmpSwComponent);
  ImageIndex  := FImageMap.ValueOf(component.ComponentClass);

  if ImageIndex = -1 then
  begin
    package   := GetComponentPackage(component.ComponentClass);
    if Length(package) > 0 then
      ImageIndex  := LoadComponentImage(package, component.ComponentClass);
  end;

  if ImageIndex = -2 then
    ImageIndex := -1;
end;


function TCmpSwStyleVisitor.GetComponentPackage(const AClassName: String): String;
var
  packageServices:    IOTAPackageServices;
  packageIndex:       Integer;
  componentIndex:     Integer;

begin
  Result          := '';
  packageServices := (BorlandIDEServices as IOTAPackageServices);

  for packageIndex := Pred(packageServices.PackageCount) downto 0 do
  begin
    for componentIndex := Pred(packageServices.ComponentCount[packageIndex]) downto 0 do
    begin
      if SameText(packageServices.ComponentNames[packageIndex, componentIndex],
                  AClassName) then
      begin
        Result := packageServices.PackageNames[packageIndex];

        { Delphi 7 doesn't add the .bpl extension, BDS 2006 does, let's just
          take the safe route and check }
        if not SameText(ExtractFileExt(Result), '.bpl') then
          Result  := Result + '.bpl';

        Break;
      end;
    end;
  end;
end;


function TCmpSwStyleVisitor.LoadComponentImage(const APackageName, AClassName: String): Integer;
var
  packageHandle:    THandle;
  bitmapHandle:     THandle;
  bitmap:           Graphics.TBitmap;

begin
  Result        := -1;
  packageHandle := LoadLibrary(PChar(APackageName));

  if packageHandle <> 0 then
  try
    { BDS includes 16x16 versions of the component bitmaps, try those first }
    bitmapHandle  := LoadBitmap(packageHandle, PChar(AClassName + '16'));
    if bitmapHandle = 0 then
      bitmapHandle  := LoadBitmap(packageHandle, PChar(AClassName));

    if bitmapHandle <> 0 then
    begin
      bitmap  := Graphics.TBitmap.Create();
      try
        bitmap.Handle := bitmapHandle;

        if (bitmap.Width <> FImageList.Width) or
           (bitmap.Height <> FImageList.Height) then
        begin
          ResizeBitmap(bitmap, FImageList.Width, FImageList.Height);
        end;

        Result        := FImageList.AddMasked(bitmap, bitmap.TransparentColor);
      finally
        FreeAndNil(bitmap);
      end;
    end;
  finally
    FreeLibrary(packageHandle);
  end;

  if Result = -1 then
    Result := -2;

  FImageMap.Add(AClassName, Result);
end;


procedure TCmpSwStyleVisitor.ResizeBitmap(const ABitmap: Graphics.TBitmap;
                                          const AWidth, AHeight: Integer);
var
  tempBitmap:     Graphics.TBitmap;

begin
  tempBitmap  := Graphics.TBitmap.Create();
  try
    tempBitmap.PixelFormat  := pf24bit;
    tempBitmap.Width        := AWidth;
    tempBitmap.Height       := AHeight;

    tempBitmap.Canvas.CopyRect(Rect(0, 0, AWidth, AHeight), ABitmap.Canvas,
                               Rect(0, 0, ABitmap.Width, ABitmap.Height));

    ABitmap.Assign(tempBitmap);
  finally
    FreeAndNil(tempBitmap);
  end;
end;


{ TfrmCmpSwDialog }
function TfrmCmpSwDialog.InternalExecute(): TBaseSwItemList;
begin
  FClassFilteredList  := TBaseSwItemList.Create();
  FClassFilter        := TCmpSwComponentClassFilter.Create();
  FFilterCheckBoxes   := TObjectList.Create();
  FOtherGroup         := TCmpSwFilterGroup.Create(nil);
  try
    ClassFilter.Groups  := Settings.Filter;

    FOtherGroup.Name    := 'Other';
    FOtherGroup.Enabled := False;
    FOtherGroup.Visible := False;
    
    Result              := inherited InternalExecute();
  finally
    FreeAndNil(FOtherGroup);
    FreeAndNil(FFilterCheckBoxes);
    FreeAndNil(FClassFilter);
    FreeAndNil(FClassFilteredList);
  end;
end;


procedure TfrmCmpSwDialog.FormShow(Sender: TObject);
begin
  UpdateClassFilter();
  inherited;
end;


function TfrmCmpSwDialog.CreateStyleVisitor(): TBaseSwStyleVisitor;
begin
  Result  := TCmpSwStyleVisitor.Create(ilsTypes);
end;


procedure TfrmCmpSwDialog.DrawItemText(ACanvas: TCanvas; AItem: TBaseSwItem; ARect: TRect);
var
  text:       String;
  textRect:   TRect;

begin
  inherited;

  { Calculate item text rectangle }
  text      := GetItemDisplayName(AItem);
  textRect  := ARect;
  DrawText(ACanvas.Handle, PChar(text), Length(text), textRect, DT_SINGLELINE or
           DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS or DT_CALCRECT);

  textRect.Left   := textRect.Right;
  textRect.Right  := ARect.Right - 2;

  { Draw component class text }
  ACanvas.Font.Color  := clGrayText;
  text  := (AItem as TCmpSwComponent).ComponentClass;

  DrawText(ACanvas.Handle, PChar(text), Length(text), textRect, DT_SINGLELINE or
           DT_RIGHT or DT_VCENTER or DT_END_ELLIPSIS);
end;


procedure TfrmCmpSwDialog.UpdateClassFilter();
var
  groupIndex:       Integer;
  itemIndex:        Integer;

begin
  { Update / extend the menu }
  itemIndex := 0;
  for groupIndex := 0 to Pred(ClassFilter.Groups.Count) do
  begin
    if ClassFilter.Groups[groupIndex].Visible then
    begin
      CreateFilterMenuItem(pmnItemsFilters, ClassFilter.Groups[groupIndex], itemIndex);
      Inc(itemIndex);
    end;
  end;

  CreateFilterMenuItem(pmnItemsFilters, FOtherGroup, itemIndex);
  Inc(itemIndex);

  { Remove excess menu items }
  for groupIndex := Pred(pmnItemsFilters.Count) downto itemIndex do
    pmnItemsFilters.Delete(groupIndex);



  itemIndex := 0;
  for groupIndex := 0 to Pred(ClassFilter.Groups.Count) do
  begin
    if not ClassFilter.Groups[groupIndex].Visible then
    begin
      CreateFilterMenuItem(pmnMoreFilters.Items, ClassFilter.Groups[groupIndex], itemIndex);
      Inc(itemIndex);
    end;
  end;

  CreateFilterMenuItem(pmnMoreFilters.Items, FOtherGroup, itemIndex);
  Inc(itemIndex);

  for groupIndex := Pred(pmnMoreFilters.Items.Count) downto itemIndex do
    pmnMoreFilters.Items.Delete(groupIndex);

    
  BuildFilterCheckboxes();
  btnMoreFilters.Visible  := (pmnMoreFilters.Items.Count > 0);

  ClassFilteredList.Clone(ItemList);
  
  ClassFilter.FilterUnmatched := FOtherGroup.Enabled;
  ClassFilter.FilterList(ClassFilteredList);
  SortList();
end;


function TfrmCmpSwDialog.GetBaseItemList(): TBaseSwItemList;
begin
  Result  := ClassFilteredList;
end;


procedure TfrmCmpSwDialog.BuildFilterCheckboxes();
var
  checkBox:         TCheckBox;
  checkBoxTop:      Integer;
  childIndex:       Integer;
  group:            TCmpSwFilterGroup;
  groupIndex:       Integer;

begin
  for childIndex := Pred(gbFilters.ControlCount) downto 0 do
    if gbFilters.Controls[childIndex] is TCheckBox then
      gbFilters.Controls[childIndex].Free;

  // #ToDo3 (MvR) 11-12-2007: get rid of a few "magic" numbers
  checkBoxTop := 16;

  for groupIndex := 0 to Pred(ClassFilter.Groups.Count) do
  begin
    group := ClassFilter.Groups[groupIndex];

    if group.Visible then                                
    begin
      checkBox          := TCheckBox.Create(Self);
      checkBox.Top      := checkBoxTop;
      checkBox.Left     := 12;
      checkBox.Caption  := StringReplace(group.Name, '&', '&&', [rfReplaceAll]);
      checkBox.Checked  := not group.Enabled;
      checkBox.Tag      := Integer(group);
      checkBox.OnClick  := FilterCheckBoxClick;
      checkBox.Width    := gbFilters.ClientWidth - 24;
      checkBox.Anchors  := [akLeft, akTop, akRight];
      checkBox.Parent   := gbFilters;

      Inc(checkBoxTop, 20);
    end;
  end;

  btnMoreFilters.BringToFront();

  pnlFilters.Height := (2 * pnlFilters.BorderWidth) + checkBoxTop + 11;
  pnlButtons.Top    := pnlFilters.Top + pnlFilters.Height;
  sbStatus.Top      := MaxInt;
end;


function TfrmCmpSwDialog.CreateFilterMenuItem(AParent: TMenuItem; AGroup: TCmpSwFilterGroup; AItemIndex: Integer): TMenuItem;
begin
  if (AItemIndex = -1) or (AItemIndex >= AParent.Count) then
  begin
    Result          := TMenuItem.Create(Self);
    Result.OnClick  := FilterMenuItemClick;

    AParent.Add(Result);
  end else
    Result          := AParent[AItemIndex];

  Result.Caption  := StringReplace(AGroup.Name, '&', '&&', [rfReplaceAll]);
  Result.Checked  := not AGroup.Enabled;
  Result.Tag      := Integer(AGroup);
end;


procedure TfrmCmpSwDialog.FilterCheckBoxClick(Sender: TObject);
var
  checkBox:   TCheckBox;
  group:      TCmpSwFilterGroup;

begin
  checkBox          := (Sender as TCheckBox);
  group             := TCmpSwFilterGroup(checkBox.Tag);

  if checkBox.Checked = group.Enabled then
  begin
    group.Enabled     := not checkBox.Checked;

    UpdateClassFilter();
    UpdateSubFilters();
  end;
end;


procedure TfrmCmpSwDialog.FilterMenuItemClick(Sender: TObject);
var
  menuItem:   TMenuItem;
  group:      TCmpSwFilterGroup;

begin
  menuItem          := (Sender as TMenuItem);
  group             := TCmpSwFilterGroup(menuItem.Tag);

  group.Enabled     := menuItem.Checked;
  menuItem.Checked  := not menuItem.Checked;

  UpdateClassFilter();
  UpdateSubFilters();
end;


procedure TfrmCmpSwDialog.btnMoreFiltersClick(Sender: TObject);
var
  buttonPos:    TPoint;

begin
  buttonPos := btnMoreFilters.ClientToScreen(Point(btnMoreFilters.Width, 0));
  pmnMoreFilters.Popup(buttonPos.X, buttonPos.Y);
end;


procedure TfrmCmpSwDialog.LoadSettings();
begin
  Self.ClientWidth  := Settings.Dialog.Width;
  Self.ClientHeight := Settings.Dialog.Height;
  MRUList.Assign(Settings.Dialog.MRUList);

  inherited LoadSettings();
end;


procedure TfrmCmpSwDialog.SaveSettings();
begin
  // #ToDo2 (MvR) 12-12-2007: save 'sort by ...'

  Settings.Dialog.Width   := Self.ClientWidth;
  Settings.Dialog.Height  := Self.ClientHeight;
  Settings.Dialog.MRUList.Assign(MRUList);
  Settings.Save();

  inherited SaveSettings();
end;



function TfrmCmpSwDialog.AllowEmptyResult(): Boolean;
begin
  Result := Settings.AllowEmptyResult;
end;


function TfrmCmpSwDialog.ColorsEnabled(): Boolean;
begin
  Result := inherited ColorsEnabled();
end;


procedure TfrmCmpSwDialog.SortExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := True;
  SortList();
  UpdateSubFilters();
end;


function SortByName(Item1, Item2: Pointer): Integer;
begin
  Result  := CompareText(TCmpSwComponent(Item1).Name, TCmpSwComponent(Item2).Name);
end;


function SortByType(Item1, Item2: Pointer): Integer;
begin
  Result  := CompareText(TCmpSwComponent(Item1).ComponentClass, TCmpSwComponent(Item2).ComponentClass);
end;


procedure TfrmCmpSwDialog.SortList();
begin
  if actSortByType.Checked then
    ClassFilteredList.Sort(SortByType)
  else
    ClassFilteredList.Sort(SortByName);
end;


procedure TfrmCmpSwDialog.btnConfigurationClick(Sender: TObject);
begin
  if TfrmCmpSwConfiguration.Execute() then
  begin
    UpdateClassFilter();
    UpdateSubFilters();
  end;
end;

end.
