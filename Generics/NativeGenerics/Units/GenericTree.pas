unit GenericTree;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericList;

type
//  TGTreeNode<TItem> = class;

//  TGTreeNodeList = TGList<TGTreeNode>;

  TGTreeNode<TItem> = class
//    Childs: ;
    Value: TItem;
    procedure Clear;
  end;

  TGTree<TItem> = class
  //  TopItem: TGTreeNode;
    procedure Clear;
  end;

implementation

{ TGTreeNode }

procedure TGTreeNode<TItem>.Clear;
begin
  //Childs.Clear;
end;

{ TGTree }

procedure TGTree<TItem>.Clear;
begin
  //if Assigned(TopItem) then TopItem.Clear;
end;

end.
