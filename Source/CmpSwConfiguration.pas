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
  StdCtrls;

type
  TfrmCmpSwConfiguration = class(TForm)
    actAdd:                                     TAction;
    actEdit:                                    TAction;
    actRemove:                                  TAction;
    alMain:                                     TActionList;
    btnAdd:                                     TButton;
    btnCancel:                                  TButton;
    btnDefault:                                 TButton;
    btnEdit:                                    TButton;
    btnOk:                                      TButton;
    btnRemove:                                  TButton;
    chkAllowEmptyResults:                       TCheckBox;
    dlgColor:                                   TColorDialog;
    imgAbout:                                   TImage;
    lbFilters:                                  TListBox;
    lblBugReport:                               TLabel;
    lblVersion:                                 TLabel;
    pcConfiguration:                            TPageControl;
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
  private
    function InternalExecute(): Boolean;

    procedure LoadSettings();
    procedure SaveSettings();

    procedure RefreshFilters();
  public
    class function Execute(): Boolean;
  end;

implementation
uses
  ShellAPI,
  Windows,

  CmpSwSettings, CmpSwFilters;


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
begin
  //
end;


procedure TfrmCmpSwConfiguration.actEditExecute(Sender: TObject);
begin
  //
end;


procedure TfrmCmpSwConfiguration.actRemoveExecute(Sender: TObject);
begin
  //
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

end.
