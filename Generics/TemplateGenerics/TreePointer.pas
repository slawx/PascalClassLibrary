unit TreePointer;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = Pointer;
{$INCLUDE 'TreeGenericInterface.tpl'}

type
  TGTreePointer = class(TGTree)
  end;

  TTreeNodePointer = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'TreeGenericImplementation.tpl'}


end.
