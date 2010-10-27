unit IntegerList;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Integer;
{$INCLUDE 'GenericListInterface.tpl'}

type
  TIntegerList = class(TGList)
  end;

implementation

{$INCLUDE 'GenericListImplementation.tpl'}


end.
