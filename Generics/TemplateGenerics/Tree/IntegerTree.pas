unit IntegerTree;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = Integer;
{$INCLUDE 'GenericTreeInterface.tpl'}

type
  TIntegerTree = class(TGTree)
  end;

  TIntegerTreeNode = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'GenericTreeImplementation.tpl'}


end.
