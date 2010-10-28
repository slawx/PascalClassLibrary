unit TreeObject;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = TObject;
{$INCLUDE 'TreeGenericInterface.tpl'}

type
  TTreeObject = class(TGTree)
  end;

  TTreeNodeObject = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'TreeGenericImplementation.tpl'}


end.
