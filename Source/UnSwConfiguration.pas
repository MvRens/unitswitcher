{: Contains the configuration dialog.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit UnSwConfiguration;

interface
uses
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  StdCtrls;

type
  TfrmUnSwConfiguration = class(TForm)
    btnCancel:                                  TButton;
    btnDataModuleColor:                         TButton;
    btnDefault:                                 TButton;
    btnFormColor:                               TButton;
    btnOk:                                      TButton;
    btnProjectColor:                            TButton;
    btnUnitColor:                               TButton;
    chkAllowEmptyResults:                       TCheckBox;
    chkCustomColor:                             TCheckBox;
    dlgColor:                                   TColorDialog;
    imgAbout:                                   TImage;
    lblBugReport:                               TLabel;
    lblDataModuleColor:                         TLabel;
    lblFormColor:                               TLabel;
    lblProjectColor:                            TLabel;
    lblShortcutKeys:                            TLabel;
    lblUnitColor:                               TLabel;
    lblVersion:                                 TLabel;
    pcConfiguration:                            TPageControl;
    pnlCustomColor:                             TPanel;
    tsAbout:                                    TTabSheet;
    tsGeneral:                                  TTabSheet;

    procedure btnDefaultClick(Sender: TObject);
    procedure chkCustomColorClick(Sender: TObject);
    procedure lblBugReportClick(Sender: TObject);
    procedure lblShortcutKeysClick(Sender: TObject);
    procedure PickColor(Sender: TObject);
  private
    FLabels:        array[0..3] of TLabel;

    function InternalExecute(): Boolean;

    procedure LoadSettings();
    procedure SaveSettings();
  public
    class function Execute(): Boolean;
  end;

implementation
uses
  ShellAPI,
  Windows,

  UnSwSettings,
  UnSwShortcuts;


{$R *.dfm}


{ TfrmUnSwConfiguration }
class function TfrmUnSwConfiguration.Execute(): Boolean;
begin
  with Self.Create(nil) do
  try
    pcConfiguration.ActivePage  := tsGeneral;

    Result  := InternalExecute();
  finally
    Free();
  end;
end;


function TfrmUnSwConfiguration.InternalExecute(): Boolean;
var
  iLabel:     Integer;

begin
  for iLabel := 0 to Pred(pnlCustomColor.ControlCount) do
    with pnlCustomColor do
    if (Controls[iLabel] is TLabel) and
       (Controls[iLabel].Tag > 0) then
      FLabels[Pred(Controls[iLabel].Tag)] := TLabel(Controls[iLabel]);

  LoadSettings();
  Result  := (ShowModal() = mrOk);
  if Result then
    SaveSettings();
end;


procedure TfrmUnSwConfiguration.lblBugReportClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'mailto:support@x2software.net', nil, nil, SW_SHOWNORMAL);
end;


procedure TfrmUnSwConfiguration.lblShortcutKeysClick(Sender: TObject);
begin
  TfrmUnSwShortcuts.Execute();
end;


procedure TfrmUnSwConfiguration.LoadSettings();
begin
  chkCustomColor.Checked        := Settings.Colors.Enabled;
  lblDataModuleColor.Font.Color := Settings.Colors.DataModules;
  lblFormColor.Font.Color       := Settings.Colors.Forms;
  lblProjectColor.Font.Color    := Settings.Colors.ProjectSource;
  lblUnitColor.Font.Color       := Settings.Colors.Units;
  chkAllowEmptyResults.Checked  := Settings.Filter.AllowEmptyResult;
end;


procedure TfrmUnSwConfiguration.SaveSettings();
begin
  Settings.Colors.Enabled           := chkCustomColor.Checked;
  Settings.Colors.DataModules       := lblDataModuleColor.Font.Color;
  Settings.Colors.Forms             := lblFormColor.Font.Color;
  Settings.Colors.ProjectSource     := lblProjectColor.Font.Color;
  Settings.Colors.Units             := lblUnitColor.Font.Color;
  Settings.Filter.AllowEmptyResult  := chkAllowEmptyResults.Checked;
  Settings.Save();
end;


procedure TfrmUnSwConfiguration.btnDefaultClick(Sender: TObject);
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


procedure TfrmUnSwConfiguration.chkCustomColorClick(Sender: TObject);
const
  Colors:     array[Boolean] of TColor  = (clBtnFace, clWindow);

begin
  pnlCustomColor.Enabled  := chkCustomColor.Checked;
  pnlCustomColor.Color    := Colors[pnlCustomColor.Enabled];
end;


procedure TfrmUnSwConfiguration.PickColor(Sender: TObject);
var
  typeLabel:      TLabel;
  
begin
  typeLabel       := FLabels[Pred((Sender as TComponent).Tag)];
  dlgColor.Color  := typeLabel.Font.Color;
  if dlgColor.Execute() then
    typeLabel.Font.Color  := dlgColor.Color;
end;

end.
