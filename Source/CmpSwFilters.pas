unit CmpSwFilters;

interface
uses
  Classes,
  Contnrs,

  BaseSwFilters,
  BaseSwObjects;


type
  TCmpSwFilterGroup = class(TCollectionItem)
  private
    FEnabled:             Boolean;
    FFilter:              TStrings;
    FFilterChanged:       Boolean;
    FFilterMasks:         TObjectList;
    FIncludeDescendants:  Boolean;
    FName:                String;
    FVisible:             Boolean;
  protected
    procedure FilterChange(Sender: TObject);

    function ContainsMask(const AFilter: String): Boolean;
    procedure UpdateFilters();

    property FilterMasks:   TObjectList read FFilterMasks;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy(); override;

    function Matches(const AValue: String): Boolean;

    property Enabled:               Boolean     read FEnabled             write FEnabled;
    property Filter:                TStrings    read FFilter;
    property IncludeDescendants:    Boolean     read FIncludeDescendants  write FIncludeDescendants;
    property Name:                  String      read FName                write FName;
    property Visible:               Boolean     read FVisible             write FVisible;
  end;


  TCmpSwMatchResult = (mrNoMatch, mrDisabledMatch, mrMatch);

  TCmpSwFilterGroups = class(TCollection)
  private
    function GetItem(Index: Integer): TCmpSwFilterGroup;
    procedure SetItem(Index: Integer; Value: TCmpSwFilterGroup);
  public
    constructor Create();

    function Add(): TCmpSwFilterGroup;
    function Matches(const AValue: String; AMatchDisabled: Boolean): TCmpSwMatchResult;

    property Items[Index: Integer]: TCmpSwFilterGroup read GetItem  write SetItem; default;
  end;


  TCmpSwComponentClassFilter  = class(TBaseSwItemFilter)
  private
    FFilterUnmatched:   Boolean;
    FGroups:            TCmpSwFilterGroups;
  protected
    procedure VisitItem(const AItem: TBaseSwItem); override;
  public
    constructor Create();
    destructor Destroy(); override;

    property FilterUnmatched:   Boolean             read FFilterUnmatched write FFilterUnmatched;
    property Groups:            TCmpSwFilterGroups  read FGroups;
  end;


implementation
uses
  Masks,
  SysUtils,

  CmpSwObjects, Dialogs;


const
  MaskChars = ['*', '?', '['];
  

{ TCmpSwFilterGroup }
constructor TCmpSwFilterGroup.Create(Collection: TCollection);
begin
  inherited;

  FFilterMasks                  := TObjectList.Create(True);
  FFilter                       := TStringList.Create();
  TStringList(FFilter).OnChange := FilterChange;

  FEnabled  := True;
  FVisible  := False;
end;


destructor TCmpSwFilterGroup.Destroy();
begin
  FreeAndNil(FFilter);
  FreeAndNil(FFilterMasks);

  inherited;
end;


function TCmpSwFilterGroup.Matches(const AValue: String): Boolean;
var
  filterIndex:    Integer;
  valueClass:     TClass;
  filterClass:    TClass;

begin
  Result  := False;
  UpdateFilters();

  for filterIndex := Pred(Filter.Count) downto 0 do
  begin
    if Assigned(Filter.Objects[filterIndex]) then
    begin
      Result  := TMask(Filter.Objects[filterIndex]).Matches(AValue);
    end else
    begin
      Result  := SameText(Filter[filterIndex], AValue);

      if (not Result) and IncludeDescendants then
      begin
        filterClass := GetClass(Filter[filterIndex]);
        if Assigned(filterClass) then
        begin
          valueClass  := GetClass(AValue);
          if Assigned(valueClass) then
            Result := valueClass.InheritsFrom(filterClass);
        end;
      end;
    end;

    if Result then
      Break;
  end;
end;


procedure TCmpSwFilterGroup.FilterChange(Sender: TObject);
begin
  FFilterChanged  := True;
end;


function TCmpSwFilterGroup.ContainsMask(const AFilter: String): Boolean;
var
  charIndex:    Integer;

begin
  Result := False;

  for charIndex := Length(AFilter) downto 1 do
    if AFilter[charIndex] in MaskChars then
    begin
      Result := True;
      Break;
    end;
end;


procedure TCmpSwFilterGroup.UpdateFilters();
var
  filterIndex:    Integer;
  mask:           TMask;

begin
  if not FFilterChanged then
    Exit;

  FilterMasks.Clear();

  for filterIndex := Pred(Filter.Count) downto 0 do
  begin
    if ContainsMask(Filter[filterIndex]) then
    begin
      mask  := TMask.Create(Filter[filterIndex]);
      
      FilterMasks.Add(mask);
      Filter.Objects[filterIndex] := mask;
    end else
      Filter.Objects[filterIndex] := nil;
  end;

  FFilterChanged  := False;
end;


{ TCmpSwFilterGroups }
constructor TCmpSwFilterGroups.Create;
begin
  inherited Create(TCmpSwFilterGroup);
end;


function TCmpSwFilterGroups.Add: TCmpSwFilterGroup;
begin
  Result  := TCmpSwFilterGroup(inherited Add);
end;


function TCmpSwFilterGroups.GetItem(Index: Integer): TCmpSwFilterGroup;
begin
  Result  := TCmpSwFilterGroup(inherited GetItem(Index));
end;


procedure TCmpSwFilterGroups.SetItem(Index: Integer; Value: TCmpSwFilterGroup);
begin
  inherited SetItem(Index, Value);
end;


function TCmpSwFilterGroups.Matches(const AValue: String; AMatchDisabled: Boolean): TCmpSwMatchResult;
var
  itemIndex:    Integer;

begin
  Result  := mrNoMatch;

  for itemIndex := Pred(Count) downto 0 do
  begin
    if AMatchDisabled or Items[itemIndex].Enabled then
    begin
      if Items[itemIndex].Matches(AValue) then
      begin
        if Items[itemIndex].Enabled then
          Result := mrMatch
        else
          Result := mrDisabledMatch;

        Break;
      end;
    end;
  end;
end;


{ TCmpSwComponentClassFilter }
constructor TCmpSwComponentClassFilter.Create();
begin
  inherited;

  FGroups := TCmpSwFilterGroups.Create();
end;


destructor TCmpSwComponentClassFilter.Destroy();
begin
  FreeAndNil(FGroups);

  inherited;
end;


procedure TCmpSwComponentClassFilter.VisitItem(const AItem: TBaseSwItem);
begin
  case Groups.Matches(TCmpSwComponent(AItem).ComponentClass, FilterUnmatched) of
    mrMatch:
      FilterItem(AItem);
    mrNoMatch:
      if FilterUnmatched then
        FilterItem(AItem);
  end;
end;

end.
