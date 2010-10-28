unit TreeInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = Integer;
{$INCLUDE 'TreeGenericInterface.tpl'}

type
  TTreeInteger = class(TGTree)
  end;

  TTreeNodeInteger = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'TreeGenericImplementation.tpl'}


end.
