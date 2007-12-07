{: Provides a base for hooking IDE actions.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}

{$ASSERTIONS ON}

unit BaseSwClient;

interface
uses
  ActnList,
  Classes;

type
  PHookedAction = ^THookedAction;
  THookedAction = record
    Action:         TContainedAction;
    OldOnExecute:   TNotifyEvent;
    NewOnExecute:   TNotifyEvent;
  end;


  TBaseSwitcherHook = class(TObject)
  private
    FHookedActions: TList;
  protected
    function GetHookedActionIndex(AAction: TContainedAction): Integer;
    function GetHookedAction(AAction: TContainedAction): PHookedAction;

    procedure HookAction(AAction: TContainedAction; AOnExecute: TNotifyEvent);
    function HookIDEAction(const AName: String; AOnExecute: TNotifyEvent): TContainedAction;
    procedure UnhookActionIndex(AIndex: Integer);
    procedure UnhookAction(AAction: TContainedAction);

    procedure OldActionExecute(AAction: TObject);
  public
    constructor Create();
    destructor Destroy(); override;
  end;


implementation
uses
  SysUtils,
  ToolsAPI;


{ TBaseSwitcherHook }
constructor TBaseSwitcherHook.Create();
begin
  inherited;

  FHookedActions  := TList.Create();
end;


destructor TBaseSwitcherHook.Destroy();
var
  actionIndex:    Integer;

begin
  for actionIndex := Pred(FHookedActions.Count) downto 0 do
    UnhookActionIndex(actionIndex);

  FreeAndNil(FHookedActions);

  inherited;
end;


function TBaseSwitcherHook.GetHookedActionIndex(AAction: TContainedAction): Integer;
var
  actionIndex:  Integer;

begin
  Result  := -1;

  for actionIndex := Pred(FHookedActions.Count) downto 0 do
    if PHookedAction(FHookedActions[actionIndex]).Action = AAction then
    begin
      Result  := actionIndex;
      Break;
    end;
end;


function TBaseSwitcherHook.GetHookedAction(AAction: TContainedAction): PHookedAction;
var
  actionIndex:  Integer;

begin
  Result      := nil;
  actionIndex := GetHookedActionIndex(AAction);

  if actionIndex > -1 then
    Result    := FHookedActions[actionIndex];
end;


procedure TBaseSwitcherHook.HookAction(AAction: TContainedAction; AOnExecute: TNotifyEvent);
var
  hookedAction:   PHookedAction;

begin
  Assert(GetHookedActionIndex(AAction) = -1, 'Action is already hooked');

  New(hookedAction);
  hookedAction^.Action        := AAction;
  hookedAction^.OldOnExecute  := AAction.OnExecute;
  hookedAction^.NewOnExecute  := AOnExecute;
  FHookedActions.Add(hookedAction);

  AAction.OnExecute := AOnExecute;
end;


function TBaseSwitcherHook.HookIDEAction(const AName: String; AOnExecute: TNotifyEvent): TContainedAction;
var
  actionIndex:    Integer;
  ntaServices:    INTAServices;
  action:         TContainedAction;

begin
  Result  := nil;
  Assert(Assigned(BorlandIDEServices), 'BorlandIDEServices not available.');
  Assert(Supports(BorlandIDEServices, INTAServices, ntaServices),
                  'BorlandIDEServices does not support the ' +
                  'INTAServices interface.');

  for actionIndex := 0 to Pred(ntaServices.ActionList.ActionCount) do
  begin
    action  := ntaServices.ActionList.Actions[actionIndex];
    if action.Name = AName then
    begin
      Result  := action;
      HookAction(action, AOnExecute);
      Break;
    end;
  end;

  Assert(Assigned(Result), AName + ' action is not available in the IDE.');
end;


procedure TBaseSwitcherHook.UnhookActionIndex(AIndex: Integer);
var
  hookedAction:   PHookedAction;
  action:         TContainedAction;
//  onExecute:      TNotifyEvent;

begin
  hookedAction  := FHookedActions[AIndex];
  action        := TContainedAction(hookedAction^.Action);
//  onExecute     := action.OnExecute;

//  if onExecute = hookedAction^.NewOnExecute then
    action.OnExecute  := hookedAction^.OldOnExecute;

  Dispose(hookedAction);
  FHookedActions.Delete(AIndex);
end;


procedure TBaseSwitcherHook.UnhookAction(AAction: TContainedAction);
var
  actionIndex:  Integer;

begin
  actionIndex := GetHookedActionIndex(AAction);
  if actionIndex > -1 then
    UnhookActionIndex(actionIndex);
end;


procedure TBaseSwitcherHook.OldActionExecute(AAction: TObject);
var
  hookedAction:   PHookedAction;

begin
  if AAction is TContainedAction then
  begin
    hookedAction  := GetHookedAction(TContainedAction(AAction));

    if Assigned(hookedAction) and Assigned(hookedAction^.NewOnExecute) then
      hookedAction^.NewOnExecute(AAction);
  end;
end;

end.

