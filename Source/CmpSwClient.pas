{: Connects ComponentSwitcher to the IDE.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit CmpSwClient;

// #ToDo2 (MvR) 9-12-2007: Ctrl activates the designer guidelines in BDS 2006
//                         and up, which get drawn over our dialog. messy.

interface
uses
  ActnList,
  Classes,
  Dialogs,
  SysUtils,
  ToolsAPI,

  BaseSwClient;


type
  TComponentSwitcherHook = class(TBaseSwitcherHook)
  protected
    function ActiveModule(): IOTAModule;
    function ActiveEditor(): IOTAEditor;

    procedure NewUpdate(Sender: TObject);
    procedure NewExecute(Sender: TObject);
  public
    constructor Create();
  end;


implementation
uses
  BaseSwObjects,
  CmpSwDialog,
  CmpSwObjects, Windows;


{ TComponentSwitcherHook}
constructor TComponentSwitcherHook.Create();
begin
  inherited;

  try
    HookIDEAction('SearchFindCommand', NewExecute, NewUpdate);
  except
    on E:EAssertionFailed do
      ShowMessage('Error while loading ComponentSwitcher: ' + E.Message);
  end;
end;


function TComponentSwitcherHook.ActiveModule(): IOTAModule;
begin
  Result := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
end;


function TComponentSwitcherHook.ActiveEditor(): IOTAEditor;
var
  module:   IOTAModule;

begin
  Result  := nil;
  module  := ActiveModule();

  if Assigned(module) then
    Result  := activeModule.CurrentEditor;
end;


function SortByName(Item1, Item2: Pointer): Integer;
begin
  Result  := CompareText(TCmpSwComponent(Item1).Name, TCmpSwComponent(Item2).Name)
end;


procedure TComponentSwitcherHook.NewExecute(Sender: TObject);
var
  editor:           IOTAEditor;
  formEditor:       IOTAFormEditor;
  formComponent:    IOTAComponent;
  componentIndex:   Integer;
  component:        IOTAComponent;
  itemList:         TBaseSwItemList;
  item:             TCmpSwComponent;
  selectedItems:    TBaseSwItemList;

begin
  editor  := ActiveEditor();
  if Assigned(editor) and Supports(editor, IOTAFormEditor, formEditor) then
  begin
    formComponent := formEditor.GetRootComponent;
    itemList      := TBaseSwItemList.Create();

    for componentIndex := Pred(formComponent.GetComponentCount) downto 0 do
    begin
      component := formComponent.GetComponent(componentIndex);
      item      := TCmpSwComponent.TryCreate(component);

      if Assigned(item) then
        itemList.Add(item);
    end;

    if itemList.Count > 0 then
    begin
      itemList.Sort(SortByName);

      (*
      ps := (borlandideservices as IOTAPackageServices);
      for pi := 0 to Pred(ps.PackageCount) do
      begin
        for ci := 0 to Pred(ps.ComponentCount[pi]) do
        begin
          itemList.Add(TTestItem.Create(ps.PackageNames[pi] + ' - ' + ps.ComponentNames[pi, ci]));
        end;
      end;
      *)

      selectedItems := TfrmCmpSwDialog.Execute(itemList);

      if Assigned(selectedItems) then
      begin
        for componentIndex := 0 to Pred(selectedItems.Count) do
        begin
          item  := TCmpSwComponent(selectedItems[componentIndex]);
          item.Activate(componentIndex = 0);
        end;
      end;
    end;
  end;
end;


procedure TComponentSwitcherHook.NewUpdate(Sender: TObject);
var
  editor:           IOTAEditor;

begin
  { BDS 2006 with the Embedded Designer disables the Find action }
  editor  := ActiveEditor();

  if Assigned(editor) and Supports(editor, IOTAFormEditor) then
    (Sender as TCustomAction).Enabled := True
  else
    OldActionUpdate(Sender);
end;

end.
