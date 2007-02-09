{: Encapsulates the settings.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit UnSwSettings;

interface
uses
  Classes,
  Graphics,
  Registry;

type
  TUnSwBaseSettings   = class(TObject)
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


  TUnSwDialogSort     = (dsName, dsType);

  TUnSwDialogSettings = class(TUnSwBaseSettings)
  private
    FHeight:                Integer;
    FIncludeDataModules:    Boolean;
    FIncludeForms:          Boolean;
    FIncludeProjectSource:  Boolean;
    FIncludeUnits:          Boolean;
    FMRUList:               TStrings;
    FPrefix:                String;
    FSort:                  TUnSwDialogSort;
    FWidth:                 Integer;
  protected
    function GetKeyName(const AName: String): String; override;
    procedure Load(const ARegistry: TRegistry); override;
    procedure Save(const ARegistry: TRegistry); override;
  public
    constructor Create(const APrefix: String);
    destructor Destroy(); override;

    property Height:                Integer             read FHeight                write FHeight;
    property IncludeDataModules:    Boolean             read FIncludeDataModules    write FIncludeDataModules;
    property IncludeForms:          Boolean             read FIncludeForms          write FIncludeForms;
    property IncludeProjectSource:  Boolean             read FIncludeProjectSource  write FIncludeProjectSource;
    property IncludeUnits:          Boolean             read FIncludeUnits          write FIncludeUnits;
    property MRUList:               TStrings            read FMRUList               write FMRUList;
    property Sort:                  TUnSwDialogSort     read FSort                  write FSort;
    property Width:                 Integer             read FWidth                 write FWidth;
  end;


  TUnSwColorSettings  = class(TUnSwBaseSettings)
  private
    FDataModules:       TColor;
    FEnabled:           Boolean;
    FForms:             TColor;
    FProjectSource:     TColor;
    FUnits:             TColor;
  protected
    procedure Load(const ARegistry: TRegistry); override;
    procedure Save(const ARegistry: TRegistry); override;
  public
    property DataModules:     TColor  read FDataModules   write FDataModules;
    property Enabled:         Boolean read FEnabled       write FEnabled;
    property Forms:           TColor  read FForms         write FForms;
    property ProjectSource:   TColor  read FProjectSource write FProjectSource;
    property Units:           TColor  read FUnits         write FUnits;
  end;


  TUnSwFilterSettings = class(TUnSwBaseSettings)
  private
    FAllowEmptyResults:   Boolean;
  protected
    procedure Load(const ARegistry: TRegistry); override;
    procedure Save(const ARegistry: TRegistry); override;
  public
    property AllowEmptyResult:  Boolean read FAllowEmptyResults write FAllowEmptyResults;
  end;


  TUnSwSettings       = class(TObject)
  private
    FColors:                TUnSwColorSettings;
    FFilter:                TUnSwFilterSettings;
    FFormsDialog:           TUnSwDialogSettings;
    FUnitsDialog:           TUnSwDialogSettings;

    FRegistryKey:           String;
  protected
    procedure Load();
  public
    constructor Create();
    destructor Destroy(); override;

    procedure ResetDefaults(const AColorsOnly: Boolean = False);
    procedure Save();

    property Colors:          TUnSwColorSettings  read FColors      write FColors;
    property Filter:          TUnSwFilterSettings read FFilter      write FFilter;
    property FormsDialog:     TUnSwDialogSettings read FFormsDialog write FFormsDialog;
    property UnitsDialog:     TUnSwDialogSettings read FUnitsDialog write FUnitsDialog;
  end;


  function Settings(): TUnSwSettings;


implementation
uses
  SysUtils,
  ToolsAPI,
  Windows;

var
  GSettings:      TUnSwSettings;


function Settings(): TUnSwSettings;
begin
  if not Assigned(GSettings) then
    GSettings := TUnSwSettings.Create();

  Result  := GSettings;
end;


{ TUnSwBaseSettings }
function TUnSwBaseSettings.GetKeyName(const AName: String): String;
begin
  Result  := AName;
end;


procedure TUnSwBaseSettings.ReadBoolDef(const ARegistry: TRegistry;
                                        var AValue: Boolean;
                                        const AName: String);
begin
  if ARegistry.ValueExists(GetKeyName(AName)) then
    AValue  := ARegistry.ReadBool(GetKeyName(AName));
end;


procedure TUnSwBaseSettings.ReadColorDef(const ARegistry: TRegistry;
                                         var AValue: TColor;
                                         const AName: String);
begin
  if ARegistry.ValueExists(GetKeyName(AName)) then
    AValue  := TColor(ARegistry.ReadInteger(GetKeyName(AName)));
end;


procedure TUnSwBaseSettings.ReadIntegerDef(const ARegistry: TRegistry;
                                           var AValue: Integer;
                                           const AName: String);
begin
  if ARegistry.ValueExists(GetKeyName(AName)) then
    AValue  := ARegistry.ReadInteger(GetKeyName(AName));
end;


procedure TUnSwBaseSettings.WriteBool(const ARegistry: TRegistry;
                                      const AValue: Boolean;
                                      const AName: String);
begin
  ARegistry.WriteBool(GetKeyName(AName), AValue);
end;


procedure TUnSwBaseSettings.WriteColor(const ARegistry: TRegistry;
                                       const AValue: TColor;
                                       const AName: String);
begin
  WriteInteger(ARegistry, Integer(AValue), AName);
end;


procedure TUnSwBaseSettings.WriteInteger(const ARegistry: TRegistry;
                                         const AValue: Integer;
                                         const AName: String);
begin
  ARegistry.WriteInteger(GetKeyName(AName), AValue);
end;


{ TUnSwDialogSettings }
constructor TUnSwDialogSettings.Create(const APrefix: String);
begin
  inherited Create();

  FMRUList  := TStringList.Create();
  FPrefix   := APrefix;

  TStringList(FMRUList).CaseSensitive := False
end;


destructor TUnSwDialogSettings.Destroy();
begin
  FreeAndNil(FMRUList);
   
  inherited;
end;


function TUnSwDialogSettings.GetKeyName(const AName: String): String;
begin
  Result  := FPrefix + AName;
end;

procedure TUnSwDialogSettings.Load(const ARegistry: TRegistry);
var
  iSort:      Integer;
  sMRU:       String;

begin
  // Conversion v0.1 -> v0.2
  if ARegistry.ValueExists('IncludeDataModules') then
  begin
    ARegistry.RenameValue('IncludeDataModules',   'UnitsIncludeDataModules');
    ARegistry.RenameValue('IncludeForms',         'UnitsIncludeForms');
    ARegistry.RenameValue('IncludeProjectSource', 'UnitsIncludeProjectSource');

    ARegistry.RenameValue('FormsDialogHeight',    'FormsHeight');
    ARegistry.RenameValue('FormsDialogWidth',     'FormsWidth');

    ARegistry.RenameValue('UnitsDialogHeight',    'UnitsHeight');
    ARegistry.RenameValue('UnitsDialogWidth',     'UnitsWidth');
  end;

  ReadBoolDef(ARegistry,    FIncludeDataModules,    'IncludeDataModules');
  ReadBoolDef(ARegistry,    FIncludeForms,          'IncludeForms');
  ReadBoolDef(ARegistry,    FIncludeProjectSource,  'IncludeProjectSource');
  ReadBoolDef(ARegistry,    FIncludeUnits,          'IncludeUnits');

  ReadIntegerDef(ARegistry, FWidth,   'Width');
  ReadIntegerDef(ARegistry, FHeight,  'Height');

  iSort := Integer(FSort);
  ReadIntegerDef(ARegistry, iSort,    'Sort');
  FSort := TUnSwDialogSort(iSort);

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


procedure TUnSwDialogSettings.Save(const ARegistry: TRegistry);
var
  sMRU:       String;

begin
  WriteBool(ARegistry,    FIncludeDataModules,    'IncludeDataModules');
  WriteBool(ARegistry,    FIncludeForms,          'IncludeForms');
  WriteBool(ARegistry,    FIncludeProjectSource,  'IncludeProjectSource');
  WriteBool(ARegistry,    FIncludeUnits,          'IncludeUnits');

  WriteInteger(ARegistry, FWidth,         'Width');
  WriteInteger(ARegistry, FHeight,        'Height');

  WriteInteger(ARegistry, Integer(FSort), 'Sort');

  if FMRUList.Count > 0 then
  begin
    sMRU  := FMRUList.Text;
    ARegistry.WriteBinaryData(GetKeyName('MRU'), PChar(sMRU)^, Length(sMRU));
  end else
    ARegistry.DeleteValue(GetKeyName('MRU'));
end;


{ TUnSwColorSettings }
procedure TUnSwColorSettings.Load(const ARegistry: TRegistry);
begin
  ReadBoolDef(ARegistry,  FEnabled,       'ColorEnabled');
  ReadColorDef(ARegistry, FDataModules,   'ColorDataModules');
  ReadColorDef(ARegistry, FForms,         'ColorForms');
  ReadColorDef(ARegistry, FProjectSource, 'ColorProjectSource');
  ReadColorDef(ARegistry, FUnits,         'ColorUnits');
end;


procedure TUnSwColorSettings.Save(const ARegistry: TRegistry);
begin
  WriteBool(ARegistry,    FEnabled,       'ColorEnabled');
  WriteColor(ARegistry,   FDataModules,   'ColorDataModules');
  WriteColor(ARegistry,   FForms,         'ColorForms');
  WriteColor(ARegistry,   FProjectSource, 'ColorProjectSource');
  WriteColor(ARegistry,   FUnits,         'ColorUnits');
end;


{ TUnSwFilterSettings }
procedure TUnSwFilterSettings.Load(const ARegistry: TRegistry);
begin
  ReadBoolDef(ARegistry,  FAllowEmptyResults, 'AllowEmptyResults');
end;


procedure TUnSwFilterSettings.Save(const ARegistry: TRegistry);
begin
  WriteBool(ARegistry,    FAllowEmptyResults, 'AllowEmptyResults');
end;


{ TUnSwSettings }
constructor TUnSwSettings.Create();
begin
  inherited Create();

  FRegistryKey  := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey() +
                   '\UnitSwitcher';

  FColors       := TUnSwColorSettings.Create();
  FFilter       := TUnSwFilterSettings.Create();
  FFormsDialog  := TUnSwDialogSettings.Create('Forms');
  FUnitsDialog  := TUnSwDialogSettings.Create('Units');

  ResetDefaults();
  Load();
end;


destructor TUnSwSettings.Destroy();
begin
  FreeAndNil(FUnitsDialog);
  FreeAndNil(FFormsDialog);
  FreeAndNil(FFilter);
  FreeAndNil(FColors);

  inherited;
end;


procedure TUnSwSettings.ResetDefaults(const AColorsOnly: Boolean);

  procedure ResetDialog(const ADialog: TUnSwDialogSettings);
  begin
    ADialog.IncludeDataModules    := True;
    ADialog.IncludeForms          := True;
    ADialog.IncludeProjectSource  := True;
    ADialog.IncludeUnits          := True;
    ADialog.Sort                  := dsName;
    ADialog.Width                 := 300;
    ADialog.Height                := 425;
  end;

  
begin
  if not AColorsOnly then
  begin
    ResetDialog(FFormsDialog);
    ResetDialog(FUnitsDialog);
  end;

  FColors.Enabled           := True;
  FColors.DataModules       := RGB( 35, 120,  35);  // Green
  FColors.Forms             := RGB( 50,  70, 120);  // Blue
  FColors.ProjectSource     := RGB(120, 120,  35);  // Yellow
  FColors.Units             := RGB(150,  35,  35);  // Red

  FFilter.AllowEmptyResult  := False;
end;

procedure TUnSwSettings.Load();
var
  ideRegistry:      TRegistry;

begin
  ideRegistry := TRegistry.Create();
  with ideRegistry do
  try
    RootKey := HKEY_CURRENT_USER;

    if OpenKey(FRegistryKey, False) then
    begin
      FColors.Load(ideRegistry);
      FFilter.Load(ideRegistry);
      FFormsDialog.Load(ideRegistry);
      FUnitsDialog.Load(ideRegistry);

      CloseKey();
    end;
  finally
    Free();
  end;
end;


procedure TUnSwSettings.Save();
var
  ideRegistry:      TRegistry;

begin
  ideRegistry := TRegistry.Create();
  with ideRegistry do
  try
    RootKey := HKEY_CURRENT_USER;

    if OpenKey(FRegistryKey, True) then
    begin
      FColors.Save(ideRegistry);
      FFilter.Save(ideRegistry);
      FFormsDialog.Save(ideRegistry);
      FUnitsDialog.Save(ideRegistry);

      CloseKey();
    end;
  finally
    Free();
  end;
end;


initialization
finalization
  FreeAndNil(GSettings);

end.
