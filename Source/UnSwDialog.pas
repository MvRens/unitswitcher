unit UnSwDialog;

// #ToDo1 Store dialog settings

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

  UnSwObjects,
  UnSwFilters;

type
  TUnSwIconVisitor  = class(TUnSwNoRefIntfObject, IUnSwVisitor)
  private
    FImageIndex:        Integer;
  protected
    procedure VisitModule(const AUnit: TUnSwModuleUnit);
    procedure VisitProject(const AUnit: TUnSwProjectUnit);
  public
    property ImageIndex:      Integer read FImageIndex;
  end;

  TfrmUnSwDialog = class(TForm)
    btnCancel:                                  TButton;
    btnOK:                                      TButton;
    chkDataModules:                             TCheckBox;
    chkForms:                                   TCheckBox;
    chkProjectSource:                           TCheckBox;
    edtSearch:                                  TEdit;
    ilsTypes:                                   TImageList;
    lstUnits:                                   TListBox;
    pnlButtons:                                 TPanel;
    pnlIncludeTypes:                            TPanel;
    pnlMain:                                    TPanel;
    pnlSearch:                                  TPanel;
    sbStatus:                                   TStatusBar;

    procedure edtSearchChange(Sender: TObject);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TypeFilterChange(Sender: TObject);
    procedure lstUnitsData(Control: TWinControl; Index: Integer; var Data: string);
    procedure lstUnitsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  private
    FLoading:               Boolean;
    FUnitList:              TUnSwUnitList;
    FActiveUnit:            TUnSwUnit;
    FFormsOnly:             Boolean;

    FTypeFilteredList:      TUnSwUnitList;
    FInputFilteredList:     TUnSwUnitList;

    FTypeFilter:            TUnSwUnitTypeFilter;
    FInputFilter:           TUnSwUnitSimpleFilter;

    FIconVisitor:           TUnSwIconVisitor;

    function InternalExecute(): TUnSwUnit;
    procedure UpdateTypeFilter();
    procedure UpdateList();

    function GetActiveUnit(): TUnSwUnit;
  public
    class function Execute(const AUnits: TUnSwUnitList;
                           const AFormsOnly: Boolean;
                           const AActive: TUnSwUnit = nil): TUnSwUnit;
  end;

implementation
uses
  DIalogs,
  SysUtils,
  Graphics;


{$R *.dfm}


{ TUnSwIconVisitor }
procedure TUnSwIconVisitor.VisitModule(const AUnit: TUnSwModuleUnit);
begin
  case AUnit.UnitType of
    swutUnit:         FImageIndex := 1;
    swutForm:         FImageIndex := 2;
    swutDataModule:   FImageIndex := 3;
  else
                      FImageIndex := 0;
  end;
end;

procedure TUnSwIconVisitor.VisitProject(const AUnit: TUnSwProjectUnit);
begin
  FImageIndex := 4;
end;


{ TfrmUnSwDialog }
class function TfrmUnSwDialog.Execute(const AUnits: TUnSwUnitList;
                                      const AFormsOnly: Boolean;
                                      const AActive: TUnSwUnit): TUnSwUnit;
begin
  with Self.Create(nil) do
  try
    FUnitList   := AUnits;
    FActiveUnit := AActive;
    FFormsOnly  := AFormsOnly;
    Result      := InternalExecute();
  finally
    Free();
  end;
end;

function SortByName(Item1, Item2: Pointer): Integer;
begin
  Result  := CompareText(TUnSwUnit(Item1).Name, TUnSwUnit(Item2).Name)
end;

function TfrmUnSwDialog.InternalExecute(): TUnSwUnit;
begin
  Result              := nil;

  FTypeFilteredList   := TUnSwUnitList.Create();
  FInputFilteredList  := TUnSwUnitList.Create();
  FTypeFilter         := TUnSwUnitTypeFilter.Create(FTypeFilteredList);
  FInputFilter        := TUnSwUnitSimpleFilter.Create(FInputFilteredList);
  try
    if not FFormsOnly then
    begin
      FLoading  := True;
      try
        chkForms.Checked          := FTypeFilter.IncludeForms;
        chkDataModules.Checked    := FTypeFilter.IncludeDataModules;
        chkProjectSource.Checked  := FTypeFilter.IncludeProjectSource;
      finally
        FLoading  := False;
      end;
    end else
      pnlIncludeTypes.Visible   := False;

    UpdateTypeFilter();

    FIconVisitor  := TUnSwIconVisitor.Create();
    try
      if Self.ShowModal() = mrOk then
        Result  := GetActiveUnit();
    finally
      FreeAndNil(FIconVisitor);
    end;
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

  FInputFilteredList.Clone(FTypeFilteredList);
  FInputFilteredList.AcceptVisitor(FInputFilter);

  lstUnits.Count  := FInputFilteredList.Count;
  if FInputFilteredList.Count > 0 then
  begin
    if Assigned(pActive) then
      lstUnits.ItemIndex  := FInputFilteredList.IndexOf(pActive);

    if lstUnits.ItemIndex = -1 then
      lstUnits.ItemIndex  := 0;
  end;
end;

procedure TfrmUnSwDialog.UpdateTypeFilter();
begin
  FTypeFilter.IncludeUnits          := not FFormsOnly;
  FTypeFilter.IncludeForms          := (FFormsOnly or chkForms.Checked);
  FTypeFilter.IncludeDataModules    := ((not FFormsOnly) and chkDataModules.Checked);
  FTypeFilter.IncludeProjectSource  := ((not FFormsOnly) and chkProjectSource.Checked);

  FTypeFilteredList.Clone(FUnitList);
  FTypeFilteredList.AcceptVisitor(FTypeFilter);
  FTypeFilteredList.Sort(SortByName);
  UpdateList();
end;

function TfrmUnSwDialog.GetActiveUnit(): TUnSwUnit;
begin
  Result  := FActiveUnit;
  if not Assigned(Result) then
  begin
    if lstUnits.ItemIndex > -1 then
      Result  := FInputFilteredList[lstUnits.ItemIndex];
  end else
    FActiveUnit := nil;
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
        begin
          if lstUnits.ItemIndex > 0 then
            lstUnits.ItemIndex  := Pred(lstUnits.ItemIndex);

          Key := 0;
        end;
      VK_DOWN:
        begin
          if lstUnits.ItemIndex < Pred(lstUnits.Items.Count) then
            lstUnits.ItemIndex  := Succ(lstUnits.ItemIndex);

          Key := 0;
        end;
    end;
end;

procedure TfrmUnSwDialog.TypeFilterChange(Sender: TObject);
begin
  if not FLoading then
    UpdateTypeFilter();
end;

procedure TfrmUnSwDialog.lstUnitsData(Control: TWinControl; Index: Integer;
                                      var Data: string);
begin
  Data  := FInputFilteredList[Index].Name;
end;

procedure TfrmUnSwDialog.lstUnitsDrawItem(Control: TWinControl; Index: Integer;
                                          Rect: TRect; State: TOwnerDrawState);
var
  pUnit:      TUnSwUnit;
  rText:      TRect;
  sText:      String;

begin
  with TListBox(Control) do
  begin
    pUnit := FInputFilteredList[Index];

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

    pUnit.AcceptVisitor(FIconVisitor);
    ilsTypes.Draw(Canvas, rText.Left, rText.Top, FIconVisitor.ImageIndex);

    Inc(rText.Left, ilsTypes.Width + 4);
    sText := pUnit.Name;
    DrawText(Canvas.Handle, PChar(sText), Length(sText), rText, DT_SINGLELINE or
             DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
  end;
end;

end.
