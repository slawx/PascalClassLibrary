unit SetString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TSetIndex = Integer;
  TSetItem = string;
{$INCLUDE '..\Generic\SetInterface.tpl'}

type

  { TSetString }

  TSetString = class(TGSet)
  end;

implementation

{$INCLUDE '..\Generic\SetImplementation.tpl'}


end.
