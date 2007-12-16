{: Contains the filter configuration dialog.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit CmpSwFilterConfiguration;

interface
uses
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  StdCtrls,

  CmpSwFilters;


type
  TfrmCmpSwFilterConfiguration = class(TForm)
    btnCancel:                                  TButton;
    btnOk:                                      TButton;
    bvlHelp:                                    TBevel;
    chkIncludeDescendants:                      TCheckBox;
    chkVisible:                                 TCheckBox;
    edtName:                                    TEdit;
    gbMain:                                     TGroupBox;
    lblFilter:                                  TLabel;
    lblHelp:                                    TLabel;
    lblHelpText:                                TLabel;
    lblHelpText2:                               TLabel;
    lblHelpText3:                               TLabel;
    lblName:                                    TLabel;
    mmoFilter:                                  TMemo;
    pnlButtons:                                 TPanel;
  protected
    function InternalExecute(AGroup: TCmpSwFilterGroup): Boolean;
  public
    class function Execute(AGroup: TCmpSwFilterGroup): Boolean;
  end;


implementation


{$R *.dfm}


{ TfrmCmpSwFilterConfiguration }
class function TfrmCmpSwFilterConfiguration.Execute(AGroup: TCmpSwFilterGroup): Boolean;
begin
  with Self.Create(nil) do
  try
    Result  := InternalExecute(AGroup);
  finally
    Free();
  end;
end;


function TfrmCmpSwFilterConfiguration.InternalExecute(AGroup: TCmpSwFilterGroup): Boolean;
begin
  edtName.Text                  := AGroup.Name;
  chkIncludeDescendants.Checked := AGroup.IncludeDescendants;
  chkVisible.Checked            := AGroup.Visible;
  mmoFilter.Lines.Assign(AGroup.Filter);

  Result  := (ShowModal() = mrOk);

  if Result then
  begin
    AGroup.Name               := edtName.Text;
    AGroup.IncludeDescendants := chkIncludeDescendants.Checked;
    AGroup.Visible            := chkVisible.Checked;
    AGroup.Filter.Assign(mmoFilter.Lines);
  end;
end;

end.

