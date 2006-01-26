unit UnSwShortcuts;

interface
uses
  Classes,
  Controls,
  Forms,
  StdCtrls;

type
  TfrmUnSwShortcuts = class(TForm)
    btnOk:                                      TButton;
    lblHeader:                                  TLabel;
  private
    procedure InternalExecute();
  public
    class procedure Execute();
  end;

implementation

{$R *.dfm}

{ TfrmUnSwShortcuts }
class procedure TfrmUnSwShortcuts.Execute();
begin
  with Self.Create(nil) do
  try
    InternalExecute();
  finally
    Free();
  end;
end;

procedure TfrmUnSwShortcuts.InternalExecute();
begin
  ShowModal();
end;

end.
