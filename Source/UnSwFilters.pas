unit UnSwFilters;

interface
uses
  UnSwObjects;

type
  TUnSwUnitFilter       = class(TUnSwNoRefIntfObject, IUnSwVisitor)
  private
    FList:      TUnSwUnitList;
  protected
    // Called by default by all other Visit methods
    procedure VisitUnit(const AUnit: TUnSwUnit); virtual;

    procedure VisitModule(const AUnit: TUnSwModuleUnit); virtual;
    procedure VisitProject(const AUnit: TUnSwProjectUnit); virtual;

    procedure FilterUnit(const AUnit: TUnSwUnit); virtual;
  public
    constructor Create(const AList: TUnSwUnitList); virtual;
  end;

  TUnSwUnitSimpleFilter = class(TUnSwUnitFilter)
  private
    FFilter:      String;

    procedure SetFilter(const Value: String);
  protected
    procedure VisitUnit(const AUnit: TUnSwUnit); override;
  public
    property Filter:      String  read FFilter  write SetFilter;
  end;

  TUnSwUnitTypeFilter   = class(TUnSwUnitFilter)
  private
    FIncludeDataModules:    Boolean;
    FIncludeForms:          Boolean;
    FIncludeProjectSource:  Boolean;
    FIncludeUnits:          Boolean;
  protected
    procedure VisitModule(const AUnit: TUnSwModuleUnit); override;
    procedure VisitProject(const AUnit: TUnSwProjectUnit); override;
  public
    constructor Create(const AList: TUnSwUnitList); override;

    property IncludeDataModules:    Boolean read FIncludeDataModules    write FIncludeDataModules;
    property IncludeForms:          Boolean read FIncludeForms          write FIncludeForms;
    property IncludeProjectSource:  Boolean read FIncludeProjectSource  write FIncludeProjectSource;
    property IncludeUnits:          Boolean read FIncludeUnits          write FIncludeUnits;
  end;

implementation
uses
  SysUtils;
  

{ TUnSwUnitFilter }
constructor TUnSwUnitFilter.Create(const AList: TUnSwUnitList);
begin
  inherited Create();

  Assert(Assigned(AList), 'List must be assigned.');
  FList := AList;
end;

procedure TUnSwUnitFilter.VisitUnit(const AUnit: TUnSwUnit);
begin
end;

procedure TUnSwUnitFilter.VisitModule(const AUnit: TUnSwModuleUnit);
begin
  VisitUnit(AUnit);
end;

procedure TUnSwUnitFilter.VisitProject(const AUnit: TUnSwProjectUnit);
begin
  VisitUnit(AUnit);
end;

procedure TUnSwUnitFilter.FilterUnit(const AUnit: TUnSwUnit);
begin
  FList.Remove(AUnit);
end;


{ TUnSwUnitSimpleFilter }
procedure TUnSwUnitSimpleFilter.VisitUnit(const AUnit: TUnSwUnit);
begin
  if (Length(FFilter) > 0) and
     (AnsiPos(FFilter, LowerCase(AUnit.Name)) = 0) then
    FilterUnit(AUnit);
end;

procedure TUnSwUnitSimpleFilter.SetFilter(const Value: String);
begin
  FFilter := LowerCase(Value);
end;


{ TUnSwUnitTypeFilter }
constructor TUnSwUnitTypeFilter.Create(const AList: TUnSwUnitList);
begin
  inherited;

  FIncludeDataModules   := True;
  FIncludeForms         := True;
  FIncludeProjectSource := True;
  FIncludeUnits         := True;
end;

procedure TUnSwUnitTypeFilter.VisitModule(const AUnit: TUnSwModuleUnit);
var
  eValidTypes:      TUnSwUnitTypes;

begin
  eValidTypes := [];

  if FIncludeDataModules then
    Include(eValidTypes, swutDataModule);

  if FIncludeForms then
    Include(eValidTypes, swutForm);

  if FIncludeUnits then
    Include(eValidTypes, swutUnit);

  if not (AUnit.UnitType in eValidTypes) then
    FilterUnit(AUnit);
end;

procedure TUnSwUnitTypeFilter.VisitProject(const AUnit: TUnSwProjectUnit);
begin
  if not FIncludeProjectSource then
    FilterUnit(AUnit);
end;

end.
