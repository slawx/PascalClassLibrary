unit ListPointer;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Pointer;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericList.inc'}

type
  TListPointer = class(TGList)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericList.inc'}


end.
