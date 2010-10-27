unit ObjectTree;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TIndexType = Integer;
  TItemType = TObject;
{$INCLUDE 'GenericTreeInterface.tpl'}

type
  TObjectGTree = class(TGTree)
  end;

  TObjectGTreeNode = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'GenericTreeImplementation.tpl'}


end.
