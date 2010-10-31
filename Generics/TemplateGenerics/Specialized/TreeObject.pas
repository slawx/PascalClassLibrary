unit TreeObject;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = TObject;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericTree.inc'}

type
  TTreeObject = class(TGTree)
  end;

  TTreeNodeObject = class(TGTreeNode)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericTree.inc'}


end.
