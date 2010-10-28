unit ListDouble;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Double;
{$INCLUDE '..\Generic\ListInterface.tpl'}

type
  TListDouble = class(TGList)
  end;

implementation

{$INCLUDE '..\Generic\ListImplementation.tpl'}


end.
