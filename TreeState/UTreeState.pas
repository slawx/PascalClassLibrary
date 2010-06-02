unit UTreeState;

{$mode delphi}

interface

uses
  Classes, SysUtils, Contnrs, StdCtrls, ComCtrls;

type

  { TTreeNodeState }

  TTreeNodeState = class(TObjectList)
    Text: string;
    Expanded: Boolean;
    Selected: Boolean;
    function Search(Text: string): TTreeNodeState;
  end;

  { TTreeViewExpandState }

  { TTreeState }

  TTreeState = class
    TopItem: TTreeNodeState;
    procedure SaveNode(Node: TTreeNode; NodeState: TTreeNodeState);
    procedure LoadNode(Node: TTreeNode; NodeState: TTreeNodeState);
    procedure SaveTree(Tree: TTreeView);
    procedure LoadTree(Tree: TTreeView);
    destructor Destroy; override;
  end;

implementation

{ TTreeState }

procedure TTreeState.SaveNode(Node: TTreeNode; NodeState: TTreeNodeState);
var
  I: Integer;
  NewNodeState: TTreeNodeState;
begin
  NodeState.Clear;
  for I := 0 to Node.Count - 1 do begin
    NewNodeState := TTreeNodeState.Create;
    NewNodeState.Text := Node.Items[I].Text;
    NewNodeState.Expanded := Node.Items[I].Expanded;
    NewNodeState.Selected := Node.Items[I].Selected;
    SaveNode(Node.Items[I], NewNodeState);
    NodeState.Add(NewNodeState);
  end;
end;

procedure TTreeState.LoadNode(Node: TTreeNode; NodeState: TTreeNodeState);
var
  I: Integer;
  FindNodeState: TTreeNodeState;
begin
  for I := 0 to Node.Count - 1 do begin
    FindNodeState := NodeState.Search(Node.Items[I].Text);
    if Assigned(FindNodeState) then begin
      Node.Items[I].Expanded := FindNodeState.Expanded;
      Node.Items[I].Selected := FindNodeState.Selected;
      LoadNode(Node.Items[I], FindNodeState);
    end;
  end;
end;

procedure TTreeState.SaveTree(Tree: TTreeView);
begin
  TopItem.Free;
  if Assigned(Tree.TopItem) then begin
    TopItem := TTreeNodeState.Create;
    SaveNode(Tree.TopItem, TopItem);
  end;
end;

procedure TTreeState.LoadTree(Tree: TTreeView);
begin
  if Assigned(Tree.TopItem) and Assigned(TopItem) then
    LoadNode(Tree.TopItem, TopItem);
end;

destructor TTreeState.Destroy;
begin
  inherited Destroy;
  TopItem.Free;
end;

{ TTreeNodeState }

function TTreeNodeState.Search(Text:string):TTreeNodeState;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (TTreeNodeState(Items[I]).Text <> Text) do Inc(I);
  if I < Count then Result := TTreeNodeState(Items[I])
    else Result := nil;
end;

end.

