unit StringTree;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TIndexType = Integer;
  TItemType = string;
{$INCLUDE 'GenericTreeInterface.tpl'}

type
  TStringGTree = class(TGTree)
  end;

  TStringGTreeNode = class(TGTreeNode)
  end;

implementation

{$INCLUDE 'GenericTreeImplementation.tpl'}


end.
