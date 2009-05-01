{: Implements the base filtering visitors.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit BaseSwFilters;

interface
uses
  Classes,
  Masks,

  BaseSwObjects;

type
  TBaseSwItemFilter             = class(TInterfacedPersistent, IBaseSwVisitor)
  private
    FList:      TBaseSwItemList;
  protected
    // IBaseSwVisitor
    procedure VisitItem(const AItem: TBaseSwItem); virtual;

    procedure FilterItem(const AItem: TBaseSwItem); virtual;
  public
    procedure FilterList(AList: TBaseSwItemList);
  end;


  TBaseSwItemSimpleFilter       = class(TBaseSwItemFilter)
  private
    FFilter:      String;
    FFilterMask:  TMask;
    FWildchars:   Boolean;

    function GetFilterMask(): TMask;
    procedure SetFilter(const Value: String);
  protected
    property FilterMask:  TMask   read GetFilterMask;
  public
    property Filter:      String  read FFilter    write SetFilter;
    property Wildchars:   Boolean read FWildchars write FWildchars;
  end;


  TBaseSwItemSimpleNameFilter   = class(TBaseSwItemSimpleFilter)
  protected
    procedure VisitItem(const AItem: TBaseSwItem); override;
  end;


implementation
uses
  SysUtils;


{ TBaseSwItemFilter }
procedure TBaseSwItemFilter.FilterList(AList: TBaseSwItemList);
begin
  FList := AList;
  try
    FList.AcceptVisitor(Self);
  finally
    FList := nil;
  end;
end;


procedure TBaseSwItemFilter.FilterItem(const AItem: TBaseSwItem);
begin
  FList.Remove(AItem);
end;


procedure TBaseSwItemFilter.VisitItem(const AItem: TBaseSwItem);
begin
end;


{ TBaseSwItemSimpleFilter }
function TBaseSwItemSimpleFilter.GetFilterMask(): TMask;
begin
  if not Assigned(FFilterMask) then
    FFilterMask := TMask.Create('*' + FFilter + '*');

  Result := FFilterMask;
end;


procedure TBaseSwItemSimpleFilter.SetFilter(const Value: String);
var
  newValue: string;

begin
  newValue  := LowerCase(Value);
  if newValue <> FFilter then
  begin
    FFilter := newValue;
    FreeAndNil(FFilterMask);
  end;
end;


{ TBaseSwItemSimpleNameFilter }
procedure TBaseSwItemSimpleNameFilter.VisitItem(const AItem: TBaseSwItem);
var
  matches:  Boolean;
  itemName: string;

begin
  if Length(Filter) > 0 then
  begin
    itemName  := LowerCase(AItem.Name);

    if Wildchars then
      matches := FilterMask.Matches(itemName)
    else
      matches := (AnsiPos(Filter, itemName) > 0);

    if not matches then
      FilterItem(AItem);
  end;
end;

end.
