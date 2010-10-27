

type
  TListIndex = TTreeIndex;
  TListItem = TTreeItem;
//{$INCLUDE 'GenericTreeInterface.tpl'}

  // TGTreeNode<TTreeIndex, TTreeItem> = class
  TGTreeNode = class

  end;

  // TGTree<TTreeIndex, TTreeItem> = class
  TGTree = class
    TopItem: TGTreeNode;
  end;
