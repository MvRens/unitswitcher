{: Implements the base filtering visitors.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit BaseSwFilters;

interface
uses
  Classes,

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

    procedure SetFilter(const Value: String);
  public
    property Filter:      String  read FFilter  write SetFilter;
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
procedure TBaseSwItemSimpleFilter.SetFilter(const Value: String);
begin
  FFilter := LowerCase(Value);
end;


{ TBaseSwItemSimpleNameFilter }
procedure TBaseSwItemSimpleNameFilter.VisitItem(const AItem: TBaseSwItem);
begin
  if (Length(Filter) > 0) and
     (AnsiPos(Filter, LowerCase(AItem.Name)) = 0) then
    FilterItem(AItem);
end;

end.
