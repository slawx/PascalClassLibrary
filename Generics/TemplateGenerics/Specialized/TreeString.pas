unit TreeString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = string;
{$INCLUDE '..\Generic\TreeInterface.tpl'}

type
  TTreeString = class(TGTree)
  end;

  TTreeNodeString = class(TGTreeNode)
  end;

implementation

{$INCLUDE '..\Generic\TreeImplementation.tpl'}


end.
