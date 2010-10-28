unit ListString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = string;
{$INCLUDE '..\Generic\ListInterface.tpl'}

type

  { TListString }

  TListString = class(TGList)
  end;

implementation

{$INCLUDE '..\Generic\ListImplementation.tpl'}


end.
