unit StringTree;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TTreeIndex = Integer;
  TTreeItem = string;
{$INCLUDE 'GenericTreeInterface.tpl'}

type
  TStringTree = class(TGTree)
  end;

  TStringTreeNode = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'GenericTreeImplementation.tpl'}


end.
