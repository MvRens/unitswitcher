{: Implements unit filtering visitors.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit UnSwFilters;

interface
uses
  Classes,

  BaseSwFilters,
  BaseSwObjects,
  UnSwObjects;


type
  TUnSwUnitSimpleFormNameFilter = class(TBaseSwItemSimpleNameFilter, IUnSwVisitor)
  protected
    procedure VisitModule(const AUnit: TUnSwModuleUnit);
    procedure VisitProject(const AUnit: TUnSwProjectUnit);
  end;


  TUnSwUnitTypeFilter           = class(TBaseSwItemFilter)
  private
    FIncludeDataModules:    Boolean;
    FIncludeForms:          Boolean;
    FIncludeProjectSource:  Boolean;
    FIncludeUnits:          Boolean;
  protected
    procedure VisitModule(const AUnit: TUnSwModuleUnit);
    procedure VisitProject(const AUnit: TUnSwProjectUnit);
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


{ TUnSwUnitSimpleFormNameFilter }
procedure TUnSwUnitSimpleFormNameFilter.VisitModule(const AUnit: TUnSwModuleUnit);
begin
  if (Length(Filter) > 0) and
     (AnsiPos(Filter, LowerCase(AUnit.FormName)) = 0) then
    FilterItem(AUnit);
end;


procedure TUnSwUnitSimpleFormNameFilter.VisitProject(const AUnit: TUnSwProjectUnit);
begin
  VisitItem(AUnit);
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
    FilterItem(AUnit);
end;


procedure TUnSwUnitTypeFilter.VisitProject(const AUnit: TUnSwProjectUnit);
begin
  if not FIncludeProjectSource then
    FilterItem(AUnit);
end;

end.
