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
    gbMain: TGroupBox;
    pnlButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
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
  Result  := (ShowModal() = mrOk);
end;

end.

