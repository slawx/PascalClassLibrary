unit TreePointer;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = Pointer;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericTree.inc'}

type
  TGTreePointer = class(TGTree)
  end;

  TTreeNodePointer = class(TGTreeNode)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericTree.inc'}


end.
