unit TreeObject;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = TObject;
{$INCLUDE '..\Generic\TreeInterface.tpl'}

type
  TTreeObject = class(TGTree)
  end;

  TTreeNodeObject = class(TGTreeNode)
  end;

implementation

{$INCLUDE '..\Generic\TreeImplementation.tpl'}


end.
