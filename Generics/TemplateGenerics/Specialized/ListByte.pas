unit ListByte;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Byte;
{$INCLUDE '..\Generic\ListInterface.tpl'}

type
  TListByte = class(TGList)
  end;
  
  TMemoryBlock = TListByte;
  // System.Move -> TListByte.Move
  // System.FillChar -> TListByte.Fill


implementation

{$INCLUDE '..\Generic\ListImplementation.tpl'}


end.
