
  TGTreeNode = class;

  TListIndex = TTreeIndex;
  TListItem = TGTreeNode;
  {$INCLUDE 'ListInterface.tpl'}

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
