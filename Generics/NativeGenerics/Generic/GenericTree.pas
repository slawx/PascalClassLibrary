unit GenericTree;

{$mode delphi}

interface

uses
  GenericList;
{
type
  TGTreeNode<T> = class
    type
      TTreeNodeType = TGTreeNode<T>;
    var
    Childs: TGList<TTreeNodeType>;
    Value: T;
    procedure Clear;
  end;

  TGTree<T> = class
    TopItem: TGTreeNode<T>;
    procedure Clear;
  end;
}

implementation

(*
{ TGTreeNode }

procedure TGTreeNode<T>.Clear;
begin
  Childs.Clear;
end;

{ TGTree }

procedure TGTree<T>.Clear;
begin
  if Assigned(TopItem) then TopItem.Clear;
end;

*)

end.
