unit TreeString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = string;
{$INCLUDE 'TreeGenericInterface.tpl'}

type
  TTreeString = class(TGTree)
  end;

  TTreeNodeString = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'TreeGenericImplementation.tpl'}


end.
