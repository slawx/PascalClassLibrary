unit SetChar;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TSetIndex = Integer;
  TSetItem = Char;
{$INCLUDE '..\Generic\SetInterface.tpl'}

type

  { TSetChar }

  TSetChar = class(TGSet)
  end;

implementation

{$INCLUDE '..\Generic\SetImplementation.tpl'}

end.
