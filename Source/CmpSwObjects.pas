unit CmpSwObjects;

interface
uses
  ToolsAPI,

  BaseSwObjects;


type
  TCmpSwComponent = class(TBaseSwItem)
  private
    FComponent:   IOTAComponent;
    FName:        String;
  protected
    function GetName(): String; override;
  public
    constructor Create(AComponent: IOTAComponent);

    procedure Activate(const AClearSelection: Boolean); 

    class function TryCreate(AComponent: IOTAComponent): TCmpSwComponent;
  end;


implementation
uses
  SysUtils;


{ TCmpSwComponent }
class function TCmpSwComponent.TryCreate(AComponent: IOTAComponent): TCmpSwComponent;
begin
  Result  := TCmpSwComponent.Create(AComponent);
  if Length(Result.Name) = 0 then
    FreeAndNil(Result);
end;


constructor TCmpSwComponent.Create(AComponent: IOTAComponent);
begin
  inherited Create();

  FComponent  := AComponent;
  FComponent.GetPropValueByName('Name', FName);
end;


procedure TCmpSwComponent.Activate(const AClearSelection: Boolean);
begin
  FComponent.Select(not AClearSelection);
end;


function TCmpSwComponent.GetName(): String;
begin
  Result  := FName;
end;

end.
