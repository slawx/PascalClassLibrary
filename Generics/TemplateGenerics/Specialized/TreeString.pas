unit TreeString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = string;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericTree.inc'}

type
  TTreeString = class(TGTree)
  end;

  TTreeNodeString = class(TGTreeNode)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericTree.inc'}


end.
