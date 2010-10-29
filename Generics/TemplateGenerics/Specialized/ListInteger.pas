unit ListInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Integer;
{$INCLUDE '..\Generic\ListInterface.tpl'}

type
  TListInteger = class(TGList)
  end;

implementation

{$INCLUDE '..\Generic\ListImplementation.tpl'}


end.