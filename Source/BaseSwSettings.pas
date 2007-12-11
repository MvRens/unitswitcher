{: Encapsulates the settings.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit BaseSwSettings;


interface
uses
  Graphics,
  Registry;

  
type
  TBaseSwSettings   = class(TObject)
  protected
    procedure Load(const ARegistry: TRegistry); virtual; abstract;
    procedure Save(const ARegistry: TRegistry); virtual; abstract;

    function GetKeyName(const AName: String): String; virtual;

    procedure ReadBoolDef(const ARegistry: TRegistry; var AValue: Boolean; const AName: String);
    procedure ReadIntegerDef(const ARegistry: TRegistry; var AValue: Integer; const AName: String);
    procedure ReadColorDef(const ARegistry: TRegistry; var AValue: TColor; const AName: String);

    procedure WriteBool(const ARegistry: TRegistry; const AValue: Boolean; const AName: String);
    procedure WriteInteger(const ARegistry: TRegistry; const AValue: Integer; const AName: String);
    procedure WriteColor(const ARegistry: TRegistry; const AValue: TColor; const AName: String);
  end;

  
implementation


{ TBaseSwSettings }
function TBaseSwSettings.GetKeyName(const AName: String): String;
begin
  Result  := AName;
end;


procedure TBaseSwSettings.ReadBoolDef(const ARegistry: TRegistry;
                                        var AValue: Boolean;
                                        const AName: String);
begin
  if ARegistry.ValueExists(GetKeyName(AName)) then
    AValue  := ARegistry.ReadBool(GetKeyName(AName));
end;


procedure TBaseSwSettings.ReadColorDef(const ARegistry: TRegistry;
                                         var AValue: TColor;
                                         const AName: String);
begin
  if ARegistry.ValueExists(GetKeyName(AName)) then
    AValue  := TColor(ARegistry.ReadInteger(GetKeyName(AName)));
end;


procedure TBaseSwSettings.ReadIntegerDef(const ARegistry: TRegistry;
                                           var AValue: Integer;
                                           const AName: String);
begin
  if ARegistry.ValueExists(GetKeyName(AName)) then
    AValue  := ARegistry.ReadInteger(GetKeyName(AName));
end;


procedure TBaseSwSettings.WriteBool(const ARegistry: TRegistry;
                                      const AValue: Boolean;
                                      const AName: String);
begin
  ARegistry.WriteBool(GetKeyName(AName), AValue);
end;


procedure TBaseSwSettings.WriteColor(const ARegistry: TRegistry;
                                       const AValue: TColor;
                                       const AName: String);
begin
  WriteInteger(ARegistry, Integer(AValue), AName);
end;


procedure TBaseSwSettings.WriteInteger(const ARegistry: TRegistry;
                                         const AValue: Integer;
                                         const AName: String);
begin
  ARegistry.WriteInteger(GetKeyName(AName), AValue);
end;

end.

