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
    OldOnUpdate:    TNotifyEvent;
    NewOnUpdate:    TNotifyEvent;
  end;


  TBaseSwitcherHook = class(TComponent)
  private
    FHookedActions: TList;
  protected
    function GetIDEActionList: TCustomActionList;

    function GetHookedActionIndex(AAction: TContainedAction): Integer;
    function GetHookedAction(AAction: TContainedAction): PHookedAction;

    procedure HookAction(AAction: TContainedAction; AOnExecute: TNotifyEvent; AOnUpdate: TNotifyEvent = nil);
    function HookIDEAction(const AName: String; AOnExecute: TNotifyEvent; AOnUpdate: TNotifyEvent = nil): TContainedAction;
    procedure UnhookActionIndex(AIndex: Integer);
    procedure UnhookAction(AAction: TContainedAction);

    procedure OldActionExecute(AAction: TObject);
    procedure OldActionUpdate(AAction: TObject);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(); reintroduce;
    destructor Destroy(); override;
  end;


implementation
uses
  SysUtils,
  ToolsAPI;


{ TBaseSwitcherHook }
constructor TBaseSwitcherHook.Create();
begin
  inherited Create(nil);

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


function TBaseSwitcherHook.GetIDEActionList: TCustomActionList;
var
  ntaServices:  INTAServices;

begin
  Assert(Assigned(BorlandIDEServices), 'BorlandIDEServices not available.');
  Assert(Supports(BorlandIDEServices, INTAServices, ntaServices),
                  'BorlandIDEServices does not support the ' +
                  'INTAServices interface.');

  Result := ntaServices.ActionList;
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


procedure TBaseSwitcherHook.HookAction(AAction: TContainedAction; AOnExecute, AOnUpdate: TNotifyEvent);
var
  hookedAction:   PHookedAction;

begin
  Assert(GetHookedActionIndex(AAction) = -1, 'Action is already hooked');

  New(hookedAction);
  hookedAction^.Action        := AAction;
  hookedAction^.OldOnExecute  := AAction.OnExecute;
  hookedAction^.NewOnExecute  := AOnExecute;
  AAction.OnExecute := AOnExecute;

  hookedAction^.OldOnUpdate   := AAction.OnUpdate;
  hookedAction^.NewOnUpdate   := AOnUpdate;
  
  if Assigned(AOnUpdate) then
    AAction.OnUpdate            := AOnUpdate;

  FHookedActions.Add(hookedAction);
  AAction.FreeNotification(Self);
end;


function TBaseSwitcherHook.HookIDEAction(const AName: String;
                                         AOnExecute, AOnUpdate: TNotifyEvent): TContainedAction;
var
  actionList:     TCustomActionList;
  actionIndex:    Integer;
  action:         TContainedAction;

begin
  Result      := nil;
  actionList  := GetIDEActionList;

  for actionIndex := 0 to Pred(actionList.ActionCount) do
  begin
    action  := actionList.Actions[actionIndex];
    if action.Name = AName then
    begin
      Result  := action;
      HookAction(action, AOnExecute, AOnUpdate);
      Break;
    end;
  end;

  Assert(Assigned(Result), AName + ' action is not available in the IDE.');
end;


procedure TBaseSwitcherHook.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent is TContainedAction) then
    UnhookAction(TContainedAction(AComponent));
end;


procedure TBaseSwitcherHook.UnhookActionIndex(AIndex: Integer);
var
  hookedAction:   PHookedAction;
  action:         TContainedAction;

begin
  hookedAction  := FHookedActions[AIndex];
  action        := TContainedAction(hookedAction^.Action);
  action.RemoveFreeNotification(Self);

  action.OnExecute  := hookedAction^.OldOnExecute;
  action.OnUpdate   := hookedAction^.OldOnUpdate;

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

    if Assigned(hookedAction) and Assigned(hookedAction^.OldOnExecute) then
      hookedAction^.OldOnExecute(AAction);
  end;
end;


procedure TBaseSwitcherHook.OldActionUpdate(AAction: TObject);
var
  hookedAction:   PHookedAction;

begin
  if AAction is TContainedAction then
  begin
    hookedAction  := GetHookedAction(TContainedAction(AAction));

    if Assigned(hookedAction) and Assigned(hookedAction^.OldOnUpdate) then
      hookedAction^.OldOnUpdate(AAction);
  end;
end;

end.

