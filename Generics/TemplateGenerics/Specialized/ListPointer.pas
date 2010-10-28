unit ListPointer;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Pointer;
{$INCLUDE '..\Generic\ListInterface.tpl'}

type
  TListPointer = class(TGList)
  end;

implementation

{$INCLUDE '..\Generic\ListImplementation.tpl'}


end.
