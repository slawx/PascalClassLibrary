
  TGTreeNode = class;

  TListIndex = TTreeIndex;
  TListItem = TGTreeNode;
  {$INCLUDE 'ListGenericInterface.tpl'}

  TTreeNodeList = class(TGList)
  end;

  // TGTreeNode<TTreeIndex, TTreeItem> = class
  TGTreeNode = class
    Childs: TTreeNodeList;
    Value: TTreeItem;
  end;

  // TGTree<TTreeIndex, TTreeItem> = class
  TGTree = class
    TopItem: TGTreeNode;
  end;
