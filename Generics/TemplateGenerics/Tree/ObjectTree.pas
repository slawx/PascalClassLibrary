unit ObjectTree;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = TObject;
{$INCLUDE 'GenericTreeInterface.tpl'}

type
  TObjectGTree = class(TGTree)
  end;

  TObjectTreeNode = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'GenericTreeImplementation.tpl'}


end.
