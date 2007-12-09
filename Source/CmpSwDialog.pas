unit CmpSwDialog;

interface
uses
  ActnList,
  Classes,
  ComCtrls,
  Controls,
  ExtCtrls,
  ImgList,
  Menus,
  StdCtrls,

  BaseSwDialog,
  BaseSwObjects;


type
  TCmpSwStyleVisitor  = class(TBaseSwStyleVisitor)
  protected
    procedure VisitItem(const AItem: TBaseSwItem); override;
  end;

  
  TfrmCmpSwDialog = class(TfrmBaseSwDialog)
  protected
    function CreateStyleVisitor(): TBaseSwStyleVisitor; override;
  end;


implementation


{$R *.dfm}


{ TCmpSwStyleVisitor }
procedure TCmpSwStyleVisitor.VisitItem(const AItem: TBaseSwItem);
begin
  inherited;

  ImageIndex  := 0;
end;


{ TfrmCmpSwDialog }
function TfrmCmpSwDialog.CreateStyleVisitor(): TBaseSwStyleVisitor;
begin
  Result  := TCmpSwStyleVisitor.Create();
end;

end.
