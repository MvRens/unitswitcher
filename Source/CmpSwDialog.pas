unit CmpSwDialog;

interface
uses
  ActnList,
  Classes,
  ComCtrls,
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
  private
    FClassFilteredList:   TBaseSwItemList;
    FClassFilter:         TCmpSwComponentClassFilter;
  protected
    function InternalExecute(): TBaseSwItemList; override;

    function CreateStyleVisitor(): TBaseSwStyleVisitor; override;
    function GetBaseItemList(): TBaseSwItemList; override;

    procedure DrawItemText(ACanvas: TCanvas; AItem: TBaseSwItem; ARect: TRect); override;

    procedure UpdateClassFilter();
  end;


implementation
uses
  SysUtils,
  ToolsAPI,

  CmpSwObjects;


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
  try
    UpdateClassFilter();
    Result  := inherited InternalExecute();
  finally
    FreeAndNil(FClassFilter);
    FreeAndNil(FClassFilteredList);
  end;
end;


function TfrmCmpSwDialog.CreateStyleVisitor(): TBaseSwStyleVisitor;
begin
  Result  := TCmpSwStyleVisitor.Create(ilsTypes);
end;


procedure TfrmCmpSwDialog.DrawItemText(ACanvas: TCanvas; AItem: TBaseSwItem; ARect: TRect);
var
  text:   String;

begin
  inherited;

  ACanvas.Font.Color  := clGrayText;
  text  := (AItem as TCmpSwComponent).ComponentClass;

  DrawText(ACanvas.Handle, PChar(text), Length(text), ARect, DT_SINGLELINE or
           DT_RIGHT or DT_VCENTER);
end;


procedure TfrmCmpSwDialog.UpdateClassFilter();
begin
//  FClassFilteredList.Clone(ItemList);
//  FClassFilter.FilterList(FClassFilteredList);
end;


function TfrmCmpSwDialog.GetBaseItemList(): TBaseSwItemList;
begin
//  Result  := FClassFilteredList;
  Result  := inherited GetBaseItemList;
end;

end.
