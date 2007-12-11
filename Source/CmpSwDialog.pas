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
  CmpSwFilters;


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
    
    procedure btnMoreFiltersClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FClassFilteredList:   TBaseSwItemList;
    FClassFilter:         TCmpSwComponentClassFilter;

    FFilterCheckBoxes:    TObjectList;
  protected
    function InternalExecute(): TBaseSwItemList; override;

    function CreateStyleVisitor(): TBaseSwStyleVisitor; override;
    function GetBaseItemList(): TBaseSwItemList; override;

    procedure LoadSettings(); override;
    procedure SaveSettings(); override;

    procedure DrawItemText(ACanvas: TCanvas; AItem: TBaseSwItem; ARect: TRect); override;
    procedure UpdateClassFilter();

    procedure BuildFilterCheckboxes();
    function CreateFilterMenuItem(AParent: TMenuItem; AGroup: TCmpSwFilterGroup; AItemIndex: Integer): TMenuItem;

    procedure FilterCheckBoxClick(Sender: TObject);
    procedure FilterMenuItemClick(Sender: TObject);

    property ClassFilter: TCmpSwComponentClassFilter  read FClassFilter;
  end;


implementation
uses
  SysUtils,
  ToolsAPI,

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
  try
    Result  := inherited InternalExecute();
  finally
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
  textRect.Right  := ARect.Right;

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
  if ClassFilter.Groups.Count = 0 then
  begin
    with ClassFilter.Groups.Add() do
    begin
      Name    := 'Actions';
      Filter.Add('TAction');
      Visible := True;
    end;

    with ClassFilter.Groups.Add() do
    begin
      Name    := 'Menu items';
      Filter.Add('TMenuItem');
      Visible := True;
    end;

    with ClassFilter.Groups.Add() do
    begin
      Name    := 'Dataset fields';

      Filter.Add('TField');
      Filter.Add('T*Field');
      Visible := True;
    end;

    with ClassFilter.Groups.Add() do
    begin
      Name    := 'DevEx Grid columns';

      Filter.Add('TcxGridDBColumn');
      Filter.Add('TcxGridColumn');
    end;

//    with ClassFilter.Groups.Add() do
//    begin
//      Name    := 'Toolbar2000 items';
//      Enabled := True;
//
//      Filter.Add('TTBXItem');
//      Filter.Add('TTBItem');
//      Filter.Add('TTBXSeparatorItem');
//      Filter.Add('TTBXNoPrefixItem');
//      Filter.Add('TTBXNoPrefixSubmenuItem');
//      Filter.Add('TTBXSubmenuItem');
//    end;

//    with ClassFilter.Groups.Add() do
//    begin
//      Name    := 'X2Software items';
//      Enabled := True;
//
//      Filter.Add('TX2GraphicContainerItem');
//    end;
  end;


  pnlFilters.Visible  := (ClassFilter.Groups.Count > 0);
  

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

  for groupIndex := Pred(pmnMoreFilters.Items.Count) downto itemIndex do
    pmnMoreFilters.Items.Delete(groupIndex);

    
  BuildFilterCheckboxes();
  btnMoreFilters.Visible  := (pmnMoreFilters.Items.Count > 0);

  FClassFilteredList.Clone(ItemList);
  ClassFilter.FilterList(FClassFilteredList);
end;


function TfrmCmpSwDialog.GetBaseItemList(): TBaseSwItemList;
begin
  Result  := FClassFilteredList;
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
      checkBox.Checked  := group.Enabled;
      checkBox.Tag      := Integer(group);
      checkBox.OnClick  := FilterCheckBoxClick;
      checkBox.Parent   := gbFilters;

      Inc(checkBoxTop, 20);
    end;
  end;

  pnlFilters.Height := (2 * pnlFilters.BorderWidth) + checkBoxTop + 11;
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
  Result.Checked  := AGroup.Enabled;
  Result.Tag      := Integer(AGroup);
end;


procedure TfrmCmpSwDialog.FilterCheckBoxClick(Sender: TObject);
var
  checkBox:   TCheckBox;
  group:      TCmpSwFilterGroup;

begin
  checkBox          := (Sender as TCheckBox);
  group             := TCmpSwFilterGroup(checkBox.Tag);

  if checkBox.Checked <> group.Enabled then
  begin
    group.Enabled     := checkBox.Checked;

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

  menuItem.Checked  := not menuItem.Checked;
  group.Enabled     := menuItem.Checked;

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

  Settings.LoadFilter(ClassFilter.Groups);

  inherited LoadSettings();
end;


procedure TfrmCmpSwDialog.SaveSettings();
begin
  Settings.Dialog.Width   := Self.ClientWidth;
  Settings.Dialog.Height  := Self.ClientHeight;
  Settings.Dialog.MRUList.Assign(MRUList);
  Settings.Save();

  Settings.SaveFilter(ClassFilter.Groups);

  inherited SaveSettings();
end;

end.
