unit PointerList;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TIndexType = Integer;
  TItemType = Pointer;
{$INCLUDE 'GenericListInterface.tpl'}

type
  TPointerGList = class(TGList)
  end;

implementation

{$INCLUDE 'GenericListImplementation.tpl'}


end.
