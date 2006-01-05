unit UnSwObjects;

interface
uses
  Classes,
  Contnrs,
  ToolsAPI;

type
  // Forward declarations
  TUnSwUnitFilter       = class;

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

  TUnSwUnit             = class(TPersistent)
  private
    FModule:        IOTAModuleInfo;

    function GetName(): String;
    function GetFileName(): String;
    function GetUnitType(): TUnSwUnitType;
  public
    constructor Create(const AModule: IOTAModuleInfo); virtual;

    property Name:          String        read GetName;
    property FileName:      String        read GetFileName;
    property UnitType:      TUnSwUnitType read GetUnitType;
  end;

  TUnSwUnitList         = class(TPersistent)
  private
    FItems:       TObjectList;

    function GetCount(): Integer;
    function GetItem(Index: Integer): TUnSwUnit;
    procedure SetItem(Index: Integer; Value: TUnSwUnit);
  public
    constructor Create();
    destructor Destroy(); override;

    function Add(const AUnit: TUnSwUnit): Integer; virtual;
    function IndexOf(const AUnit: TUnSwUnit): Integer;
    procedure Sort(Compare: TListSortCompare);

    procedure Clone(const ASource: TUnSwUnitList); virtual;
    procedure ApplyFilter(const AFilter: TUnSwUnitFilter); virtual;

    property Count:                 Integer   read GetCount;
    property Items[Index: Integer]: TUnSwUnit read GetItem
                                              write SetItem; default;
  end;

  TUnSwUnitFilter       = class(TObject)
  protected
    function IsFiltered(const AUnit: TUnSwUnit): Boolean; virtual; abstract;
  public
    constructor Create(); virtual;
  end;

  TUnSwUnitSimpleFilter = class(TUnSwUnitFilter)
  private
    FFilter:      String;

    procedure SetFilter(const Value: String);
  protected
    function IsFiltered(const AUnit: TUnSwUnit): Boolean; override;
  public
    property Filter:      String  read FFilter  write SetFilter;
  end;

  TUnSwUnitTypeFilter   = class(TUnSwUnitFilter)
  private
    FIncludeDataModules:    Boolean;
    FIncludeForms:          Boolean;
    FIncludeProjectSource:  Boolean;
  protected
    function IsFiltered(const AUnit: TUnSwUnit): Boolean; override;
  public
    constructor Create(); override;

    property IncludeDataModules:    Boolean read FIncludeDataModules    write FIncludeDataModules;
    property IncludeForms:          Boolean read FIncludeForms          write FIncludeForms;
    property IncludeProjectSource:  Boolean read FIncludeProjectSource  write FIncludeProjectSource;
  end;

implementation
uses
  SysUtils;


{ TUnSwUnit }
constructor TUnSwUnit.Create(const AModule: IOTAModuleInfo);
begin
  inherited Create();

  FModule := AModule;
end;


function TUnSwUnit.GetName(): String;
begin
  Result  := FModule.Name;
end;

function TUnSwUnit.GetFileName(): String;
begin
  Result  := FModule.FileName;
end;

function TUnSwUnit.GetUnitType(): TUnSwUnitType;
begin
  Result  := TUnSwUnitType(FModule.ModuleType);
  if (Result = swutForm) and (Length(FModule.FormName) = 0) then
    Result  := swutUnit;
end;


{ TUnSwUnitList}
constructor TUnSwUnitList.Create();
begin
  inherited Create();

  FItems  := TObjectList.Create(True);
end;


destructor TUnSwUnitList.Destroy();
begin
  FreeAndNil(FItems);

  inherited;
end;


function TUnSwUnitList.Add(const AUnit: TUnSwUnit): Integer;
begin
  Result  := FItems.Add(AUnit);
end;

function TUnSwUnitList.IndexOf(const AUnit: TUnSwUnit): Integer;
begin
  Result  := FItems.IndexOf(AUnit);
end;

procedure TUnSwUnitList.Sort(Compare: TListSortCompare);
begin
  FItems.Sort(Compare);
end;


procedure TUnSwUnitList.ApplyFilter(const AFilter: TUnSwUnitFilter);
var
  iItem:      Integer;

begin
  for iItem := Pred(Count) downto 0 do
    if AFilter.IsFiltered(Items[iItem]) then
      FItems.Delete(iItem);
end;

procedure TUnSwUnitList.Clone(const ASource: TUnSwUnitList);
var
  iItem:      Integer;

begin
  FItems.Clear();
  FItems.OwnsObjects  := False;

  for iItem := 0 to Pred(ASource.Count) do
    FItems.Add(ASource[iItem]);
end;


function TUnSwUnitList.GetCount(): Integer;
begin
  Result  := FItems.Count;
end;

function TUnSwUnitList.GetItem(Index: Integer): TUnSwUnit;
begin
  Result  := TUnSwUnit(FItems[Index]);
end;

procedure TUnSwUnitList.SetItem(Index: Integer; Value: TUnSwUnit);
begin
  FItems[Index] := Value;
end;


{ TUnSwUnitFilter }
constructor TUnSwUnitFilter.Create();
begin
  inherited Create();
end;


{ TUnSwUnitSimpleFilter }
function TUnSwUnitSimpleFilter.IsFiltered(const AUnit: TUnSwUnit): Boolean;
begin
  Result  := (Length(FFilter) > 0) and
             (AnsiPos(FFilter, LowerCase(AUnit.Name)) = 0);
end;

procedure TUnSwUnitSimpleFilter.SetFilter(const Value: String);
begin
  FFilter := LowerCase(Value);
end;


{ TUnSwUnitTypeFilter }
constructor TUnSwUnitTypeFilter.Create();
begin
  inherited;

  FIncludeDataModules   := True;
  FIncludeForms         := True;
  FIncludeProjectSource := True;
end;

function TUnSwUnitTypeFilter.IsFiltered(const AUnit: TUnSwUnit): Boolean;
var
  eValidTypes:      TUnSwUnitTypes;

begin
  eValidTypes := [swutUnit];

  if FIncludeDataModules then
    Include(eValidTypes, swutDataModule);

  if FIncludeForms then
    Include(eValidTypes, swutForm);

  if FIncludeProjectSource then
    Include(eValidTypes, swutProjUnit);

  Result  := not (AUnit.UnitType in eValidTypes);
end;

end.
