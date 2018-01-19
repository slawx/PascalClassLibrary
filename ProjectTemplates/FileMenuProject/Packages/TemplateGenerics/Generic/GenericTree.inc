{$IFDEF INTERFACE}

  TGTreeNode = class;

{$DEFINE TGListIndex := TGTreeIndex}
{$DEFINE TGListItem := TGTreeNode}
{$DEFINE TGList := TGTreeNodeList}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

  // TGTreeNode<TGTreeIndex, TGTreeItem> = class
  TGTreeNode = class
    Childs: TGTreeNodeList;
    Value: TGTreeItem;
    procedure Clear;
  end;

  // TGTree<TGTreeIndex, TGTreeItem> = class
  TGTree = class
    TopItem: TGTreeNode;
    procedure Clear;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION_USES}

  {$DEFINE IMPLEMENTATION_USES}
  {$I 'GenericList.inc'}

{$UNDEF INTERFACE_USES}
{$ENDIF}


{$IFDEF IMPLEMENTATION}
{$UNDEF IMPLEMENTATION}

{$DEFINE TGListIndex := TGTreeIndex}
{$DEFINE TGListItem := TGTreeNode}
{$DEFINE TGList := TGTreeNodeList}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

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

{$UNDEF IMPLEMENTATION}
{$ENDIF}
