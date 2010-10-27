unit PointerTree;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TIndexType = Integer;
  TItemType = Pointer;
{$INCLUDE 'GenericTreeInterface.tpl'}

type
  TPointerGTree = class(TGTree)
  end;

  TPointerGTreeNode = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'GenericTreeImplementation.tpl'}


end.
