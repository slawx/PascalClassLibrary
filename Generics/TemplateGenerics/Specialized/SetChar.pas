unit SetChar;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TSetIndex = Integer;
  TSetItem = Char;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericSet.inc'}

type

  { TSetChar }

  TSetChar = class(TGSet)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericSet.inc'}

end.
