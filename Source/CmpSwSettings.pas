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


  TCmpSwSettings        = class(TObject)
  private
    FAllowEmptyResult:      Boolean;
    FDialog:                TCmpSwDialogSettings;
    FFilter:                TCmpSwFilterGroups;

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

    property AllowEmptyResult:  Boolean               read FAllowEmptyResult  write FAllowEmptyResult;
    property Dialog:            TCmpSwDialogSettings  read FDialog            write FDialog;
    property Filter:            TCmpSwFilterGroups    read FFilter;
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


{ TCmpSwSettings }
constructor TCmpSwSettings.Create();
begin
  inherited;

  FRegistryKey  := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey() +
                   '\ComponentSwitcher';

  FDialog       := TCmpSwDialogSettings.Create();
  FFilter       := TCmpSwFilterGroups.Create();

  ResetDefaults();
  Load();
end;


destructor TCmpSwSettings.Destroy();
begin
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
      LoadFilter('\Filter', Filter);

      CloseKey();
    end;
  finally
    Free();
  end;
end;


procedure TCmpSwSettings.ResetDefaults();
begin
  AllowEmptyResult  := True;

  Dialog.Width  := 350;
  Dialog.Height := 530;

  { Fill default groups }
  Filter.Clear();
  with Filter.Add() do
  begin
    Name    := 'Actions';

    Filter.Add('TAction');
    IncludeDescendants  := True;
    Visible             := True;
  end;

  with Filter.Add() do
  begin
    Name    := 'Menu items';

    Filter.Add('TMenuItem');
    Visible := True;
  end;

  with Filter.Add() do
  begin
    Name    := 'Dataset fields';

    Filter.Add('TField');
    IncludeDescendants  := True;
    Visible             := True;
  end;

  with Filter.Add() do
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
      SaveFilter('\Filter', Filter);

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
  AGroup.Enabled            := ARegistry.ReadBool('Enabled');
  AGroup.IncludeDescendants := ARegistry.ReadBool('IncludeDescendants');
  AGroup.Visible            := ARegistry.ReadBool('Visible');

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

