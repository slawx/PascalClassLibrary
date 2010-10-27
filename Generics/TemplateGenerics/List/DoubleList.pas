unit DoubleList;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Double;
{$INCLUDE 'GenericListInterface.tpl'}

type
  TDoubleList = class(TGList)
  end;

implementation

{$INCLUDE 'GenericListImplementation.tpl'}


end.
