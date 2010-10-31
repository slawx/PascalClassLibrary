unit ListByte;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Byte;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericList.inc'}

type
  TListByte = class(TGList)
  end;
  
  TMemoryBlock = TListByte;
  // System.Move -> TListByte.Move
  // System.FillChar -> TListByte.Fill


implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericList.inc'}


end.
