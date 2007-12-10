unit CmpSwDialog;

interface
uses
  ActnList,
  Classes,
  ComCtrls,
  Controls,
  ExtCtrls,
  ImgList,
  IniFiles,
  Menus,
  StdCtrls,

  BaseSwDialog,
  BaseSwObjects;


type
  TCmpSwStyleVisitor  = class(TBaseSwStyleVisitor)
  private
    FImageList: TImageList;
    FImageMap: TStringHash;
  protected
    procedure VisitItem(const AItem: TBaseSwItem); override;

    function GetComponentPackage(const AClassName: String): String;
    function LoadComponentImage(const APackageName, AClassName: String): Integer;
  public
    constructor Create(AImageList: TImageList);
    destructor Destroy(); override;
  end;


  TfrmCmpSwDialog = class(TfrmBaseSwDialog)
  protected
    function CreateStyleVisitor(): TBaseSwStyleVisitor; override;
  end;


implementation
uses
  CommCtrl,
  SysUtils,
  ToolsAPI,
  Windows,

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
    ImageIndex := 0;
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
        Result := packageServices.PackageNames[packageIndex] + '.bpl';
        Break;
      end;
    end;
  end;
end;


function TCmpSwStyleVisitor.LoadComponentImage(const APackageName, AClassName: String): Integer;
var
  packageHandle:    THandle;
  bitmapHandle:     THandle;
  bitmap:           TBitmap;

begin
  Result        := -1;
  packageHandle := LoadLibrary(PChar(APackageName));

  if packageHandle <> 0 then
  try
    bitmapHandle  := LoadBitmap(packageHandle, PChar(AClassName));
    if bitmapHandle <> 0 then
    begin
      bitmap  ;=
      // #ToDo1 (MvR) 10-12-2007: proper transparency
      Result := ImageList_AddMasked(FImageList.Handle, bitmapHandle,
                                    GetTransparentColor(bitmapHandle));
    end;
  finally
    FreeLibrary(packageHandle);
  end;

  if Result = -1 then
    Result := -2;

  FImageMap.Add(AClassName, Result);
end;


{ TfrmCmpSwDialog }
function TfrmCmpSwDialog.CreateStyleVisitor(): TBaseSwStyleVisitor;
begin
  Result  := TCmpSwStyleVisitor.Create(ilsTypes);
end;

end.
