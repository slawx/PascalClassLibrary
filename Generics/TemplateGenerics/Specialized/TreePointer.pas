unit TreePointer;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = Pointer;
{$INCLUDE '..\Generic\TreeInterface.tpl'}

type
  TGTreePointer = class(TGTree)
  end;

  TTreeNodePointer = class(TGTreeNode)
  end;

implementation

{$INCLUDE '..\Generic\TreeImplementation.tpl'}


end.
