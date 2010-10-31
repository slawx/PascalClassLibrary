unit ListInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Integer;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericList.inc'}

type
  TListInteger = class(TGList)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericList.inc'}


end.
