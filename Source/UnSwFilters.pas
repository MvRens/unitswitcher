{: Implements unit filtering visitors.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit UnSwFilters;

interface
uses
  Classes,
  
  UnSwObjects;

type
  TUnSwUnitFilter               = class(TInterfacedPersistent, IUnSwVisitor)
  private
    FList:      TUnSwUnitList;
  protected
    // Called by default by all other Visit methods
    procedure VisitUnit(const AUnit: TUnSwUnit); virtual;

    procedure VisitModule(const AUnit: TUnSwModuleUnit); virtual;
    procedure VisitProject(const AUnit: TUnSwProjectUnit); virtual;

    procedure FilterUnit(const AUnit: TUnSwUnit); virtual;
  public
    procedure FilterList(AList: TUnSwUnitList);
  end;

  TUnSwUnitSimpleFilter         = class(TUnSwUnitFilter)
  private
    FFilter:      String;

    procedure SetFilter(const Value: String);
  public
    property Filter:      String  read FFilter  write SetFilter;
  end;

  TUnSwUnitSimpleNameFilter     = class(TUnSwUnitSimpleFilter)
  protected
    procedure VisitUnit(const AUnit: TUnSwUnit); override;
  end;

  TUnSwUnitSimpleFormNameFilter = class(TUnSwUnitSimpleNameFilter)
  protected
    procedure VisitModule(const AUnit: TUnSwModuleUnit); override;
  end;

  TUnSwUnitTypeFilter           = class(TUnSwUnitFilter)
  private
    FIncludeDataModules:    Boolean;
    FIncludeForms:          Boolean;
    FIncludeProjectSource:  Boolean;
    FIncludeUnits:          Boolean;
  protected
    procedure VisitModule(const AUnit: TUnSwModuleUnit); override;
    procedure VisitProject(const AUnit: TUnSwProjectUnit); override;
  public
    constructor Create;

    property IncludeDataModules:    Boolean read FIncludeDataModules    write FIncludeDataModules;
    property IncludeForms:          Boolean read FIncludeForms          write FIncludeForms;
    property IncludeProjectSource:  Boolean read FIncludeProjectSource  write FIncludeProjectSource;
    property IncludeUnits:          Boolean read FIncludeUnits          write FIncludeUnits;
  end;

implementation
uses
  SysUtils;
  

{ TUnSwUnitFilter }
procedure TUnSwUnitFilter.FilterList(AList: TUnSwUnitList);
begin
  FList := AList;
  try
    FList.AcceptVisitor(Self);
  finally
    FList := nil;
  end;
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
procedure TUnSwUnitSimpleFilter.SetFilter(const Value: String);
begin
  FFilter := LowerCase(Value);
end;


{ TUnSwUnitSimpleNameFilter }
procedure TUnSwUnitSimpleNameFilter.VisitUnit(const AUnit: TUnSwUnit);
begin
  if (Length(Filter) > 0) and
     (AnsiPos(Filter, LowerCase(AUnit.Name)) = 0) then
    FilterUnit(AUnit);
end;


{ TUnSwUnitSimpleFormNameFilter }
procedure TUnSwUnitSimpleFormNameFilter.VisitModule(const AUnit: TUnSwModuleUnit);
begin
  if (Length(Filter) > 0) and
     (AnsiPos(Filter, LowerCase(AUnit.FormName)) = 0) then
    FilterUnit(AUnit);
end;


{ TUnSwUnitTypeFilter }
constructor TUnSwUnitTypeFilter.Create;
begin
  inherited;

  FIncludeDataModules   := True;
  FIncludeForms         := True;
  FIncludeProjectSource := True;
  FIncludeUnits         := True;
end;


procedure TUnSwUnitTypeFilter.VisitModule(const AUnit: TUnSwModuleUnit);
var
  validTypes:       TUnSwUnitTypes;

begin
  validTypes  := [];

  if FIncludeDataModules then
    Include(validTypes, swutDataModule);

  if FIncludeForms then
    Include(validTypes, swutForm);

  if FIncludeUnits then
    Include(validTypes, swutUnit);

  if not (AUnit.UnitType in validTypes) then
    FilterUnit(AUnit);
end;


procedure TUnSwUnitTypeFilter.VisitProject(const AUnit: TUnSwProjectUnit);
begin
  if not FIncludeProjectSource then
    FilterUnit(AUnit);
end;

end.
