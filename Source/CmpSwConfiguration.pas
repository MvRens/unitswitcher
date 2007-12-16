{: Contains the configuration dialog.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit CmpSwConfiguration;

interface
uses
  ActnList,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  ImgList,
  StdCtrls,
  ToolWin,

  CmpSwFilters;


type
  TfrmCmpSwConfiguration = class(TForm)
    actAdd:                                     TAction;
    actEdit:                                    TAction;
    actRemove:                                  TAction;
    alMain:                                     TActionList;
    btnCancel:                                  TButton;
    btnDefault:                                 TButton;
    btnOk:                                      TButton;
    chkAllowEmptyResults:                       TCheckBox;
    dlgColor:                                   TColorDialog;
    ilsFilters:                                 TImageList;
    imgAbout:                                   TImage;
    lbFilters:                                  TListBox;
    lblBugReport:                               TLabel;
    lblVersion:                                 TLabel;
    pcConfiguration:                            TPageControl;
    tbFilterAdd:                                TToolButton;
    tbFilterEdit:                               TToolButton;
    tbFilterRemove:                             TToolButton;
    tbFilters:                                  TToolBar;
    tsAbout:                                    TTabSheet;
    tsGeneral:                                  TTabSheet;

    procedure btnDefaultClick(Sender: TObject);
    procedure lblBugReportClick(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actRemoveExecute(Sender: TObject);
    procedure lbFiltersClick(Sender: TObject);
    procedure lbFiltersData(Control: TWinControl; Index: Integer; var Data: String);
    procedure lbFiltersDataObject(Control: TWinControl; Index: Integer; var DataObject: TObject);
    procedure lbFiltersDblClick(Sender: TObject);
  private
    function InternalExecute(): Boolean;

    procedure LoadSettings();
    procedure SaveSettings();

    procedure RefreshFilters();
    function GetSelectedGroup(): TCmpSwFilterGroup;

    property SelectedGroup:   TCmpSwFilterGroup read GetSelectedGroup;
  public
    class function Execute(): Boolean;
  end;

implementation
uses
  ShellAPI,
  SysUtils,
  Windows,

  CmpSwFilterConfiguration,
  CmpSwSettings;


{$R *.dfm}


{ TfrmCmpSwConfiguration }
class function TfrmCmpSwConfiguration.Execute(): Boolean;
begin
  with Self.Create(nil) do
  try
    pcConfiguration.ActivePage  := tsGeneral;

    Result  := InternalExecute();
  finally
    Free();
  end;
end;


function TfrmCmpSwConfiguration.InternalExecute(): Boolean;
begin
  LoadSettings();
  RefreshFilters();

  Result  := (ShowModal() = mrOk);
  if Result then
    SaveSettings();
end;


procedure TfrmCmpSwConfiguration.lblBugReportClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'mailto:support@x2software.net', nil, nil, SW_SHOWNORMAL);
end;


procedure TfrmCmpSwConfiguration.LoadSettings();
begin
  chkAllowEmptyResults.Checked  := Settings.AllowEmptyResult;
end;


procedure TfrmCmpSwConfiguration.SaveSettings();
begin
  Settings.AllowEmptyResult := chkAllowEmptyResults.Checked;
  Settings.Save();
end;


function TfrmCmpSwConfiguration.GetSelectedGroup(): TCmpSwFilterGroup;
begin
  Result  := nil;
  if lbFilters.ItemIndex > -1 then
    Result  := TCmpSwFilterGroup(lbFilters.Items.Objects[lbFilters.ItemIndex]);
end;


procedure TfrmCmpSwConfiguration.btnDefaultClick(Sender: TObject);
begin
  if MessageBox(Self.Handle, 'Are you sure you want to revert the ' +
                             'settings? This action can not be undone.',
                             'Reset to default', MB_YESNO or MB_ICONQUESTION) = ID_YES then
  begin
    Settings.ResetDefaults();
    Settings.Save();
    LoadSettings();
  end;
end;


procedure TfrmCmpSwConfiguration.actAddExecute(Sender: TObject);
var
  newGroup:   TCmpSwFilterGroup;

begin
  newGroup  := TCmpSwFilterGroup.Create(nil);
  try
    if TfrmCmpSwFilterConfiguration.Execute(newGroup) then
    begin
      newGroup.Collection := Settings.Filter;
      RefreshFilters();
    end;
  finally
    if not Assigned(newGroup.Collection) then
      FreeAndNil(newGroup);
  end;
end;


procedure TfrmCmpSwConfiguration.actEditExecute(Sender: TObject);
var
  group:    TCmpSwFilterGroup;

begin
  group := SelectedGroup;
  if Assigned(group) then
  begin
    if TfrmCmpSwFilterConfiguration.Execute(group) then
      lbFilters.Invalidate();
  end;
end;


procedure TfrmCmpSwConfiguration.actRemoveExecute(Sender: TObject);
var
  group:    TCmpSwFilterGroup;

begin
  group := SelectedGroup;
  if Assigned(group) then
  begin
    if Application.MessageBox(PChar(Format('Do you want to remove the filter "%s"?',
                                           [group.Name])), 'Remove',
                                           MB_YESNO or MB_ICONQUESTION) = ID_YES then
    begin
      lbFilters.Items.BeginUpdate();
      try
        FreeAndNil(group);
        RefreshFilters();
      finally
        lbFilters.Items.EndUpdate();
      end;
    end;
  end;
end;


procedure TfrmCmpSwConfiguration.lbFiltersClick(Sender: TObject);
var
  itemSelected:   Boolean;

begin
  itemSelected      := (lbFilters.ItemIndex > -1);

  actEdit.Enabled   := itemSelected;
  actRemove.Enabled := itemSelected;
end;


procedure TfrmCmpSwConfiguration.RefreshFilters();
begin
  lbFilters.Count := Settings.Filter.Count;
end;


procedure TfrmCmpSwConfiguration.lbFiltersData(Control: TWinControl; Index: Integer; var Data: String);
begin
  if (Index >= 0) and (Index < Settings.Filter.Count) then
    Data  := Settings.Filter[Index].Name;
end;


procedure TfrmCmpSwConfiguration.lbFiltersDataObject(Control: TWinControl; Index: Integer; var DataObject: TObject);
begin
  if (Index >= 0) and (Index < Settings.Filter.Count) then
    DataObject  := Settings.Filter[Index];
end;


procedure TfrmCmpSwConfiguration.lbFiltersDblClick(Sender: TObject);
begin
  actEdit.Execute();
end;

end.
