{$IFDEF INTERFACE}

  TGTreeNode = class;

  TListIndex = TTreeIndex;
  TListItem = TGTreeNode;
  {$DEFINE INTERFACE}
  {$INCLUDE 'GenericList.inc'}

  TTreeNodeList = class(TGList)
  end;

  // TGTreeNode<TTreeIndex, TTreeItem> = class
  TGTreeNode = class
    Childs: TTreeNodeList;
    Value: TTreeItem;
    procedure Clear;
  end;

  // TGTree<TTreeIndex, TTreeItem> = class
  TGTree = class
    TopItem: TGTreeNode;
    procedure Clear;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{$DEFINE IMPLEMENTATION}
{$INCLUDE 'GenericList.inc'}

{ TGTreeNode }

procedure TGTreeNode.Clear;
begin
  Childs.Clear;
end;

{ TGTree }

procedure TGTree.Clear;
begin
  if Assigned(TopItem) then TopItem.Clear;
end;

{$ENDIF}
