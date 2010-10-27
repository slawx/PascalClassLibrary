unit IntegerList;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TIndexType = Integer;
  TItemType = Integer;
{$INCLUDE 'GenericListInterface.tpl'}

type
  TIntegerGList = class(TGList)
  end;

implementation

{$INCLUDE 'GenericListImplementation.tpl'}


end.
