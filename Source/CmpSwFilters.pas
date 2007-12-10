unit CmpSwFilters;

interface
uses
  BaseSwFilters,
  BaseSwObjects;


type
  TCmpSwComponentClassFilter  = class(TBaseSwItemSimpleFilter)
  protected
    procedure VisitItem(const AItem: TBaseSwItem); override;
  end;


implementation
uses
  SysUtils,

  CmpSwObjects;


{ TCmpSwComponentClassFilter }
procedure TCmpSwComponentClassFilter.VisitItem(const AItem: TBaseSwItem);
var
  componentClass:   String;

begin
  componentClass  := TCmpSwComponent(AItem).ComponentClass;

  // #ToDo1 (MvR) 10-12-2007: use a configurable list
  if SameText(componentClass, 'TMenuItem') or
     SameText(componentClass, 'TAction') or
     SameText(componentClass, 'TTBXItem') or
     SameText(componentClass, 'TTBItem') or
     SameText(componentClass, 'TTBXSeparatorItem') or
     SameText(componentClass, 'TTBXNoPrefixItem') or
     SameText(componentClass, 'TTBXNoPrefixSubmenuItem') or
     SameText(componentClass, 'TTBXSubmenuItem') or
     SameText(componentClass, 'TX2GraphicContainerItem') then
    FilterItem(AItem);
end;

end.
