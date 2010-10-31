unit TreeInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = Integer;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericTree.inc'}

type
  TTreeInteger = class(TGTree)
  end;

  TTreeNodeInteger = class(TGTreeNode)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericTree.inc'}


end.
