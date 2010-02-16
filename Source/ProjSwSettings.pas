{: Encapsulates the ProjectSwitcher settings.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit ProjSwSettings;

interface
uses
  Classes,
  Registry,

  BaseSwSettings;


type
  TProjSwDialogSettings  = class(TBaseSwSettings)
  private
    FHeight:    Integer;
    FMRUList:   TStrings;
    FWidth:     Integer;
  protected
    procedure Load(const ARegistry: TRegistry); override;
    procedure Save(const ARegistry: TRegistry); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Height:                Integer             read FHeight                write FHeight;
    property MRUList:               TStrings            read FMRUList               write FMRUList;
    property Width:                 Integer             read FWidth                 write FWidth;
  end;


  TProjSwFilterSettings  = class(TBaseSwSettings)
  private
    FAllowEmptyResult:      Boolean;
    FWildchars:             Boolean;
  protected
    procedure Load(const ARegistry: TRegistry); override;
    procedure Save(const ARegistry: TRegistry); override;
  public
    property AllowEmptyResult:  Boolean               read FAllowEmptyResult  write FAllowEmptyResult;
    property Wildchars:         Boolean               read FWildchars         write FWildchars;
  end;


  TProjSwSettings        = class(TObject)
  private
    FDialog:      TProjSwDialogSettings;
    FFilter:      TProjSwFilterSettings;

    FRegistryKey: String;
  protected
    procedure Load;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetDefaults;
    procedure Save;

    property Dialog:  TProjSwDialogSettings read FDialog  write FDialog;
    property Filter:  TProjSwFilterSettings read FFilter  write FFilter;
  end;

  function Settings: TProjSwSettings;


implementation
uses
  SysUtils,
  ToolsAPI,
  Windows;
  
  
var
  GSettings:      TProjSwSettings;


function Settings: TProjSwSettings;
begin
  if not Assigned(GSettings) then
    GSettings := TProjSwSettings.Create;

  Result  := GSettings;
end;


{ TProjSwDialogSettings }
constructor TProjSwDialogSettings.Create;
begin
  inherited Create;

  FMRUList  := TStringList.Create;
  TStringList(FMRUList).CaseSensitive := False
end;


destructor TProjSwDialogSettings.Destroy;
begin
  FreeAndNil(FMRUList);

  inherited;
end;


procedure TProjSwDialogSettings.Load(const ARegistry: TRegistry);
var
  sMRU:       String;

begin
  ReadIntegerDef(ARegistry, FWidth,   'Width');
  ReadIntegerDef(ARegistry, FHeight,  'Height');

  if ARegistry.ValueExists(GetKeyName('MRU')) then
  begin
    SetLength(sMRU, ARegistry.GetDataSize(GetKeyName('MRU')));
    if Length(sMRU) > 0 then
    begin
      ARegistry.ReadBinaryData(GetKeyName('MRU'), PChar(sMRU)^, Length(sMRU));
      FMRUList.Text := Trim(sMRU);
    end;
  end;
end;


procedure TProjSwDialogSettings.Save(const ARegistry: TRegistry);
var
  sMRU:       String;

begin
  WriteInteger(ARegistry, FWidth,         'Width');
  WriteInteger(ARegistry, FHeight,        'Height');

  if FMRUList.Count > 0 then
  begin
    sMRU  := FMRUList.Text;
    ARegistry.WriteBinaryData(GetKeyName('MRU'), PChar(sMRU)^, Length(sMRU));
  end else
    ARegistry.DeleteValue(GetKeyName('MRU'));
end;


{ TProjSwFilterSettings }
procedure TProjSwFilterSettings.Load(const ARegistry: TRegistry);
begin
  ReadBoolDef(ARegistry, FAllowEmptyResult, 'AllowEmptyResult');
  ReadBoolDef(ARegistry, FWildchars,        'Wildchars');
end;


procedure TProjSwFilterSettings.Save(const ARegistry: TRegistry);
begin
  WriteBool(ARegistry, FAllowEmptyResult, 'AllowEmptyResult');
  WriteBool(ARegistry, FWildchars,        'Wildchars');
end;


{ TProjSwSettings }
constructor TProjSwSettings.Create;
begin
  inherited;

  FRegistryKey  := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey +
                   '\ComponentSwitcher';

  FDialog       := TProjSwDialogSettings.Create;
  FFilter       := TProjSwFilterSettings.Create;

  ResetDefaults;
  Load;
end;


destructor TProjSwSettings.Destroy;
begin
  FreeAndNil(FFilter);
  FreeAndNil(FDialog);

  inherited;
end;


procedure TProjSwSettings.Load;
var
  ideRegistry:      TRegistry;

begin
  ideRegistry := TRegistry.Create;
  with ideRegistry do
  try
    RootKey := HKEY_CURRENT_USER;

    if OpenKey(FRegistryKey, False) then
    begin
      FDialog.Load(ideRegistry);
      FFilter.Load(ideRegistry);

      CloseKey;
    end;
  finally
    Free;
  end;
end;


procedure TProjSwSettings.ResetDefaults;
begin
  Filter.AllowEmptyResult := True;
  Filter.Wildchars        := True;

  Dialog.Width  := 350;
  Dialog.Height := 530;
end;


procedure TProjSwSettings.Save;
var
  ideRegistry:      TRegistry;

begin
  ideRegistry := TRegistry.Create;
  with ideRegistry do
  try
    RootKey := HKEY_CURRENT_USER;

    if OpenKey(FRegistryKey, True) then
    begin
      FDialog.Save(ideRegistry);
      FFilter.Save(ideRegistry);

      CloseKey;
    end;
  finally
    Free;
  end;
end;


initialization
finalization
  FreeAndNil(GSettings);

end.

