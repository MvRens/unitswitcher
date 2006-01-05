unit UnSwDialog;

interface
uses
  Classes,
  ComCtrls,
  Controls,
  ExtCtrls,
  Forms,
  ImgList,
  StdCtrls,
  Windows,

  UnSwObjects;

type
  TfrmUnSwDialog = class(TForm)
    edtSearch:                                  TEdit;
    ilsTypes:                                   TImageList;
    lstUnits:                                   TListBox;
    pnlMain:                                    TPanel;
    pnlSearch:                                  TPanel;
    sbStatus:                                   TStatusBar;
    Panel1: TPanel;
    btnCancel: TButton;
    btnOK: TButton;

    procedure edtSearchChange(Sender: TObject);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lstUnitsData(Control: TWinControl; Index: Integer; var Data: string);
    procedure lstUnitsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  private
    FUnitList:              TUnSwUnitList;
    FTypeFilteredList:      TUnSwUnitList;
    FInputFilteredList:     TUnSwUnitList;

    FTypeFilter:            TUnSwUnitTypeFilter;
    FInputFilter:           TUnSwUnitSimpleFilter;

    function InternalExecute(const AUnits: TUnSwUnitList): Integer;
    procedure UpdateList();

    function GetActiveUnit(): TUnSwUnit;
  public
    class function Execute(const AUnits: TUnSwUnitList): Integer;
  end;

implementation
uses
  SysUtils,
  Graphics;

{$R *.dfm}

{ TfrmUnSwDialog }
class function TfrmUnSwDialog.Execute(const AUnits: TUnSwUnitList): Integer;
begin
  with Self.Create(nil) do
  try
    Result  := InternalExecute(AUnits);
  finally
    Free();
  end;
end;


function SortByName(Item1, Item2: Pointer): Integer;
begin
  Result  := CompareText(TUnSwUnit(Item1).Name, TUnSwUnit(Item2).Name)
end;

function TfrmUnSwDialog.InternalExecute(const AUnits: TUnSwUnitList): Integer;
begin
  Result    := -1;
  FUnitList := AUnits;

  FTypeFilteredList   := TUnSwUnitList.Create();
  FInputFilteredList  := TUnSwUnitList.Create();
  FTypeFilter         := TUnSwUnitTypeFilter.Create();
  FInputFilter        := TUnSwUnitSimpleFilter.Create();
  try
    FTypeFilteredList.Clone(FUnitList);
    FTypeFilteredList.ApplyFilter(FTypeFilter);
    FTypeFilteredList.Sort(SortByName);
    UpdateList();

    Self.ShowModal();
  finally
    FreeAndNil(FInputFilter);
    FreeAndNil(FTypeFilter);
    FreeAndNil(FInputFilteredList);
    FreeAndNil(FTypeFilteredList);
  end;
end;

procedure TfrmUnSwDialog.UpdateList();
var
  pActive:      TUnSwUnit;

begin
  pActive := GetActiveUnit();
  // #ToDo1 Try to select the previous unit, otherwise select the first

  FInputFilteredList.Clone(FTypeFilteredList);
  FInputFilteredList.ApplyFilter(FInputFilter);

  lstUnits.Count  := FInputFilteredList.Count;
  if FInputFilteredList.Count > 0 then
  begin
    if Assigned(pActive) then
      lstUnits.ItemIndex  := FInputFilteredList.IndexOf(pActive);

    if lstUnits.ItemIndex = -1 then
      lstUnits.ItemIndex  := 0;
  end;
end;

function TfrmUnSwDialog.GetActiveUnit(): TUnSwUnit;
begin
  Result  := nil;
  if lstUnits.ItemIndex > -1 then
    Result  := FInputFilteredList[lstUnits.ItemIndex];
end;

procedure TfrmUnSwDialog.edtSearchChange(Sender: TObject);
begin
  FInputFilter.Filter := edtSearch.Text;
  UpdateList();
end;

procedure TfrmUnSwDialog.edtSearchKeyDown(Sender: TObject; var Key: Word;
                                          Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_UP:
        if lstUnits.ItemIndex > 0 then
          lstUnits.ItemIndex  := Pred(lstUnits.ItemIndex);
      VK_DOWN:
        if lstUnits.ItemIndex < Pred(lstUnits.Items.Count) then
          lstUnits.ItemIndex  := Succ(lstUnits.ItemIndex);
    end;
end;


procedure TfrmUnSwDialog.lstUnitsData(Control: TWinControl; Index: Integer;
                                      var Data: string);
begin
  Data  := FInputFilteredList[Index].Name;
end;

procedure TfrmUnSwDialog.lstUnitsDrawItem(Control: TWinControl; Index: Integer;
                                          Rect: TRect; State: TOwnerDrawState);
var
  iIcon:      Integer;
  rText:      TRect;
  sText:      String;

begin
  with TListBox(Control) do
  begin
    if odSelected in State then
    begin
      Canvas.Brush.Color  := clHighlight;
      Canvas.Font.Color   := clHighlightText;
    end else
    begin
      Canvas.Brush.Color  := clWindow;
      Canvas.Font.Color   := clWindowText;
    end;
    Canvas.FillRect(Rect);

    rText := Rect;
    InflateRect(rText, -2, -2);

    iIcon := 0;
    case FInputFilteredList[Index].UnitType of
      swutForm:         iIcon := 1;
      swutDataModule:   iIcon := 2;
      swutProjUnit:     iIcon := 3;
    end;
    ilsTypes.Draw(Canvas, rText.Left, rText.Top, iIcon);

    Inc(rText.Left, ilsTypes.Width + 4);
    sText := FInputFilteredList[Index].Name;
    DrawText(Canvas.Handle, PChar(sText), Length(sText), rText, DT_SINGLELINE or
             DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
  end;
end;

end.
