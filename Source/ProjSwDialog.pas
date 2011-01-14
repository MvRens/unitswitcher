unit ProjSwDialog;

interface
uses
  ActnList,
  Classes,
  ComCtrls,
  Contnrs,
  Controls,
  ExtCtrls,
  Graphics,
  ImgList,
  IniFiles,
  Menus,
  StdCtrls,
  Windows,

  BaseSwDialog,
  BaseSwObjects;


type
  TProjSwStyleVisitor  = class(TBaseSwStyleVisitor)
  protected
    procedure VisitItem(const AItem: TBaseSwItem); override;
  end;


  TfrmProjSwDialog = class(TfrmBaseSwDialog)
  protected
    function InternalExecute: TBaseSwItemList; override;

    function CreateStyleVisitor: TBaseSwStyleVisitor; override;

    function AllowEmptyResult: Boolean; override;
    function ColorsEnabled: Boolean; override;
    function Wildchars: Boolean; override;

    procedure LoadSettings; override;
    procedure SaveSettings; override;
  end;


implementation
uses
  SysUtils,
  ToolsAPI,

  ProjSwObjects,
  ProjSwSettings;


{$R *.dfm}


{ TProjSwStyleVisitor }
procedure TProjSwStyleVisitor.VisitItem(const AItem: TBaseSwItem);
begin
  ImageIndex := 0;

  if AItem is TProjSwProject then
    Bold := TProjSwProject(AItem).IsActive;
end;


{ TfrmProjSwDialog }
function TfrmProjSwDialog.InternalExecute: TBaseSwItemList;
begin
  Result  := inherited InternalExecute;
end;


function TfrmProjSwDialog.CreateStyleVisitor: TBaseSwStyleVisitor;
begin
  Result  := TProjSwStyleVisitor.Create;
end;


procedure TfrmProjSwDialog.LoadSettings;
begin
  Self.ClientWidth  := Settings.Dialog.Width;
  Self.ClientHeight := Settings.Dialog.Height;
  MRUList.Assign(Settings.Dialog.MRUList);

  inherited LoadSettings;
end;


procedure TfrmProjSwDialog.SaveSettings;
begin
  // #ToDo2 (MvR) 12-12-2007: save 'sort by ...'

  Settings.Dialog.Width   := Self.ClientWidth;
  Settings.Dialog.Height  := Self.ClientHeight;
  Settings.Dialog.MRUList.Assign(MRUList);
  Settings.Save;

  inherited SaveSettings;
end;



function TfrmProjSwDialog.AllowEmptyResult: Boolean;
begin
  Result := Settings.Filter.AllowEmptyResult;
end;


function TfrmProjSwDialog.ColorsEnabled: Boolean;
begin
  Result := inherited ColorsEnabled;
end;


function TfrmProjSwDialog.Wildchars: Boolean;
begin
  Result := Settings.Filter.Wildchars;
end;


(*
procedure TfrmProjSwDialog.btnConfigurationClick(Sender: TObject);
begin
  if TfrmProjSwConfiguration.Execute then
  begin
    SettingsChanged;
    UpdateClassFilter;
    UpdateSubFilters;
  end;
end;
*)

end.
