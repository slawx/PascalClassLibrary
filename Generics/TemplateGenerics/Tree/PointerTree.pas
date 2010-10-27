unit PointerTree;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = Pointer;
{$INCLUDE 'GenericTreeInterface.tpl'}

type
  TPointerGTree = class(TGTree)
  end;

  TPointerTreeNode = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'GenericTreeImplementation.tpl'}


end.
