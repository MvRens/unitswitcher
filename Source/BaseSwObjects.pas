{: Implements the base visitable list objects.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit BaseSwObjects;

{$I BaseSwDefines.inc}

interface
uses
  Classes,
  Contnrs;

type
  // Forward declarations
  TBaseSwItem           = class;


  IBaseSwVisitor        = interface
    ['{B081D76E-0CC7-4160-B01D-6136AE1D6711}']
    procedure VisitItem(const AItem: TBaseSwItem);
  end;


  IBaseSwVisited        = interface
    ['{9540671E-184B-4DB6-A015-27B457C74C6C}']
    procedure AcceptVisitor(const AVisitor: IBaseSwVisitor);
  end;


  TBaseSwItem           = class(TInterfacedPersistent, IBaseSwVisited)
  protected
    function GetName(): String; virtual; abstract;
  public
    // IBaseSwVisited
    procedure AcceptVisitor(const AVisitor: IBaseSwVisitor); virtual;

    property Name:          String        read GetName;
  end;


  TBaseSwItemList       = class(TInterfacedPersistent, IBaseSwVisited)
  private
    FItems:       TObjectList;
  protected
    function GetOwnsObjects(): Boolean;
    procedure SetOwnsObjects(const Value: Boolean);

    function GetCount(): Integer;
    function GetItem(Index: Integer): TBaseSwItem;
    procedure SetItem(Index: Integer; Value: TBaseSwItem);
  public
    constructor Create();
    destructor Destroy(); override;

    function Add(const AItem: TBaseSwItem): Integer; virtual;
    function IndexOf(const AItem: TBaseSwItem): Integer;
    procedure Delete(const AIndex: Integer);
    function Remove(const AItem: TBaseSwItem): Integer;

    procedure Sort(Compare: TListSortCompare);
    procedure Clone(const ASource: TBaseSwItemList); virtual;

    procedure AcceptVisitor(const AVisitor: IBaseSwVisitor);

    property Count:                 Integer     read GetCount;
    property Items[Index: Integer]: TBaseSwItem read GetItem
                                                write SetItem; default;
    property OwnsObjects:           Boolean     read GetOwnsObjects
                                                write SetOwnsObjects;
  end;

implementation
uses
  {$IFDEF DELPHI7ORLOWER}
  ActnList,
  {$ENDIF}
  SysUtils;



{ TBaseSwItem }
procedure TBaseSwItem.AcceptVisitor(const AVisitor: IBaseSwVisitor);
begin
  AVisitor.VisitItem(Self);
end;


{ TBaseSwItemList}
constructor TBaseSwItemList.Create();
begin
  inherited Create();

  FItems  := TObjectList.Create(True);
end;


destructor TBaseSwItemList.Destroy();
begin
  FreeAndNil(FItems);

  inherited;
end;


procedure TBaseSwItemList.AcceptVisitor(const AVisitor: IBaseSwVisitor);
var
  itemIndex:    Integer;

begin
  for itemIndex := Pred(Count) downto 0 do
    Items[itemIndex].AcceptVisitor(AVisitor);
end;


function TBaseSwItemList.Add(const AItem: TBaseSwItem): Integer;
begin
  Result  := FItems.Add(AItem);
end;


function TBaseSwItemList.IndexOf(const AItem: TBaseSwItem): Integer;
begin
  Result  := FItems.IndexOf(AItem);
end;


procedure TBaseSwItemList.Delete(const AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;


function TBaseSwItemList.Remove(const AItem: TBaseSwItem): Integer;
begin
  Result  := FItems.Remove(AItem);
end;


procedure TBaseSwItemList.Sort(Compare: TListSortCompare);
begin
  FItems.Sort(Compare);
end;


procedure TBaseSwItemList.Clone(const ASource: TBaseSwItemList);
var
  itemIndex:      Integer;

begin
  FItems.Clear();
  FItems.OwnsObjects  := False;

  for itemIndex := 0 to Pred(ASource.Count) do
    FItems.Add(ASource[itemIndex]);
end;


function TBaseSwItemList.GetCount(): Integer;
begin
  Result  := FItems.Count;
end;


function TBaseSwItemList.GetItem(Index: Integer): TBaseSwItem;
begin
  Result  := TBaseSwItem(FItems[Index]);
end;


procedure TBaseSwItemList.SetItem(Index: Integer; Value: TBaseSwItem);
begin
  FItems[Index] := Value;
end;


function TBaseSwItemList.GetOwnsObjects(): Boolean;
begin
  Result  := FItems.OwnsObjects;
end;


procedure TBaseSwItemList.SetOwnsObjects(const Value: Boolean);
begin
  FItems.OwnsObjects  := Value;
end;

end.
