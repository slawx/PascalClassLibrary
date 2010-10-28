unit TreeInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = Integer;
{$INCLUDE '..\Generic\TreeInterface.tpl'}

type
  TTreeInteger = class(TGTree)
  end;

  TTreeNodeInteger = class(TGTreeNode)
  end;

implementation

{$INCLUDE '..\Generic\TreeImplementation.tpl'}


end.
