{: Encapsulates the ComponentSwitcher settings.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit CmpSwSettings;

interface
uses
  Classes,
  Registry,

  BaseSwSettings,
  CmpSwFilters;


type
  TCmpSwDialogSettings  = class(TBaseSwSettings)
  private
    FHeight:    Integer;
    FMRUList:   TStrings;
    FWidth:     Integer;
  protected
    procedure Load(const ARegistry: TRegistry); override;
    procedure Save(const ARegistry: TRegistry); override;
  public
    constructor Create();
    destructor Destroy(); override;
  public
    property Height:                Integer             read FHeight                write FHeight;
    property MRUList:               TStrings            read FMRUList               write FMRUList;
    property Width:                 Integer             read FWidth                 write FWidth;
  end;


  TCmpSwFilterSettings  = class(TBaseSwSettings)
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


  TCmpSwSettings        = class(TObject)
  private
    FDialog:                TCmpSwDialogSettings;
    FFilter:                TCmpSwFilterSettings;
    FFilterGroups:          TCmpSwFilterGroups;

    FRegistryKey:           String;
  protected
    procedure Load();

    procedure LoadFilter(const AKey: String; AGroups: TCmpSwFilterGroups);
    procedure SaveFilter(const AKey: String; AGroups: TCmpSwFilterGroups);

    procedure LoadFilterGroup(ARegistry: TRegistry; AGroup: TCmpSwFilterGroup);
    procedure SaveFilterGroup(ARegistry: TRegistry; AGroup: TCmpSwFilterGroup);
  public
    constructor Create();
    destructor Destroy(); override;

    procedure ResetDefaults();
    procedure Save();

    property Dialog:            TCmpSwDialogSettings  read FDialog            write FDialog;
    property Filter:            TCmpSwFilterSettings  read FFilter;
    property FilterGroups:      TCmpSwFilterGroups    read FFilterGroups;
  end;

  function Settings(): TCmpSwSettings;


implementation
uses
  SysUtils,
  ToolsAPI,
  Windows;
  
  
var
  GSettings:      TCmpSwSettings;


function Settings(): TCmpSwSettings;
begin
  if not Assigned(GSettings) then
    GSettings := TCmpSwSettings.Create();

  Result  := GSettings;
end;


{ TCmpSwDialogSettings }
constructor TCmpSwDialogSettings.Create();
begin
  inherited Create();

  FMRUList  := TStringList.Create();
  TStringList(FMRUList).CaseSensitive := False
end;


destructor TCmpSwDialogSettings.Destroy();
begin
  FreeAndNil(FMRUList);

  inherited;
end;


procedure TCmpSwDialogSettings.Load(const ARegistry: TRegistry);
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


procedure TCmpSwDialogSettings.Save(const ARegistry: TRegistry);
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


{ TCmpSwFilterSettings }
procedure TCmpSwFilterSettings.Load(const ARegistry: TRegistry);
begin
  ReadBoolDef(ARegistry, FAllowEmptyResult, 'AllowEmptyResult');
  ReadBoolDef(ARegistry, FWildchars,        'Wildchars');
end;


procedure TCmpSwFilterSettings.Save(const ARegistry: TRegistry);
begin
  WriteBool(ARegistry, FAllowEmptyResult, 'AllowEmptyResult');
  WriteBool(ARegistry, FWildchars,        'Wildchars');
end;


{ TCmpSwSettings }
constructor TCmpSwSettings.Create();
begin
  inherited;

  FRegistryKey  := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey() +
                   '\ComponentSwitcher';

  FDialog       := TCmpSwDialogSettings.Create();
  FFilter       := TCmpSwFilterSettings.Create();
  FFilterGroups := TCmpSwFilterGroups.Create();

  ResetDefaults();
  Load();
end;


destructor TCmpSwSettings.Destroy();
begin
  FreeAndNil(FFilterGroups);
  FreeAndNil(FFilter);
  FreeAndNil(FDialog);

  inherited;
end;


procedure TCmpSwSettings.Load();
var
  ideRegistry:      TRegistry;

begin
  ideRegistry := TRegistry.Create();
  with ideRegistry do
  try
    RootKey := HKEY_CURRENT_USER;

    if OpenKey(FRegistryKey, False) then
    begin
      FDialog.Load(ideRegistry);
      FFilter.Load(ideRegistry);
      LoadFilter('\Filter', FilterGroups);

      CloseKey();
    end;
  finally
    Free();
  end;
end;


procedure TCmpSwSettings.ResetDefaults();
begin
  Filter.AllowEmptyResult := True;
  Filter.Wildchars        := True;

  Dialog.Width  := 350;
  Dialog.Height := 530;

  { Fill default groups }
  FilterGroups.Clear();
  with FilterGroups.Add() do
  begin
    Name    := 'Actions';

    Filter.Add('TAction');
    IncludeDescendants  := True;
    Visible             := True;
  end;

  with FilterGroups.Add() do
  begin
    Name    := 'Menu items';

    Filter.Add('TMenuItem');
    Visible := True;
  end;

  with FilterGroups.Add() do
  begin
    Name    := 'Dataset fields';

    Filter.Add('TField');
    IncludeDescendants  := True;
    Visible             := True;
  end;

  with FilterGroups.Add() do
  begin
    Name    := 'DevEx Grid columns';

    Filter.Add('TcxGridDBColumn');
    Filter.Add('TcxGridColumn');
  end;
end;


procedure TCmpSwSettings.Save();
var
  ideRegistry:      TRegistry;

begin
  ideRegistry := TRegistry.Create();
  with ideRegistry do
  try
    RootKey := HKEY_CURRENT_USER;

    if OpenKey(FRegistryKey, True) then
    begin
      FDialog.Save(ideRegistry);
      FFilter.Save(ideRegistry);
      SaveFilter('\Filter', FilterGroups);

      CloseKey();
    end;
  finally
    Free();
  end;
end;


procedure TCmpSwSettings.LoadFilter(const AKey: String; AGroups: TCmpSwFilterGroups);
var
  ideRegistry:      TRegistry;
  groupCount:       Integer;
  groupIndex:       Integer;
  group:            TCmpSwFilterGroup;

begin
  ideRegistry := TRegistry.Create();
  with ideRegistry do
  try
    RootKey := HKEY_CURRENT_USER;

    if OpenKey(FRegistryKey + AKey, False) then
    begin
      AGroups.Clear();
      groupCount  := 0;
      if ValueExists('Count') then
        groupCount  := ReadInteger('Count');

      CloseKey();

      for groupIndex := 0 to Pred(groupCount) do
      begin
        if OpenKey(FRegistryKey + Format('\Filter\Item%d', [groupIndex]), False) then
        begin
          group := AGroups.Add();
          LoadFilterGroup(ideRegistry, group);
          CloseKey();
        end;
      end;
    end;
  finally
    Free();
  end;
end;


procedure TCmpSwSettings.SaveFilter(const AKey: String; AGroups: TCmpSwFilterGroups);
var
  ideRegistry:      TRegistry;
  subKeys:          TStringList;
  keyIndex:         Integer;
  groupIndex:       Integer;

begin
  ideRegistry := TRegistry.Create();
  with ideRegistry do
  try
    RootKey := HKEY_CURRENT_USER;

    if OpenKey(FRegistryKey + AKey, True) then
    begin
      subKeys := TStringList.Create();
      try
        GetKeyNames(subKeys);

        for keyIndex := 0 to Pred(subKeys.Count) do
          if SameText(Copy(subKeys[keyIndex], 0, 4), 'Item') then
          begin
            DeleteKey(subKeys[keyIndex]);
          end;
      finally
        FreeAndNil(subKeys);
      end;

      WriteInteger('Count', AGroups.Count);
      CloseKey();

      for groupIndex := 0 to Pred(AGroups.Count) do
      begin
        if OpenKey(FRegistryKey + Format('\Filter\Item%d', [groupIndex]), True) then
        begin
          SaveFilterGroup(ideRegistry, AGroups[groupIndex]);
          CloseKey();
        end;
      end;

      CloseKey();
    end;
  finally
    Free();
  end;
end;


procedure TCmpSwSettings.LoadFilterGroup(ARegistry: TRegistry; AGroup: TCmpSwFilterGroup);
var
  filterText:   String;

begin
  AGroup.Name               := ARegistry.ReadString('Name');

  
  if ARegistry.ValueExists('Enabled') then
    AGroup.Enabled            := ARegistry.ReadBool('Enabled')
  else
    AGroup.Enabled            := False;

  if ARegistry.ValueExists('IncludeDescendants') then
    AGroup.IncludeDescendants := ARegistry.ReadBool('IncludeDescendants')
  else
    AGroup.IncludeDescendants := False;

  if ARegistry.ValueExists('Visible') then
    AGroup.Visible            := ARegistry.ReadBool('Visible')
  else
    AGroup.Visible            := False;


  if ARegistry.ValueExists('Filter') then
  begin
    SetLength(filterText, ARegistry.GetDataSize('Filter'));
    if Length(filterText) > 0 then
    begin
      ARegistry.ReadBinaryData('Filter', PChar(filterText)^, Length(filterText));
      AGroup.Filter.Text  := Trim(filterText);
    end;
  end;
end;


procedure TCmpSwSettings.SaveFilterGroup(ARegistry: TRegistry; AGroup: TCmpSwFilterGroup);
var
  filterText:   String;

begin
  ARegistry.WriteString('Name', AGroup.Name);
  ARegistry.WriteBool('Enabled', AGroup.Enabled);
  ARegistry.WriteBool('IncludeDescendants', AGroup.IncludeDescendants);
  ARegistry.WriteBool('Visible', AGroup.Visible);

  if AGroup.Filter.Count > 0 then
  begin
    filterText  := AGroup.Filter.Text;
    ARegistry.WriteBinaryData('Filter', PChar(filterText)^, Length(filterText));
  end else
    ARegistry.DeleteValue('Filter');
end;


initialization
finalization
  FreeAndNil(GSettings);

end.

