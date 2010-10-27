unit IntegerTree;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TIndexType = Integer;
  TItemType = Integer;
{$INCLUDE 'GenericTreeInterface.tpl'}

type
  TIntegerGTree = class(TGTree)
  end;

  TIntegerGTreeNode = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'GenericTreeImplementation.tpl'}


end.
