unit StackDouble;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TStackIndex = Integer;
  TStackItem = Double;
{$INCLUDE '..\Generic\StackInterface.tpl'}

type
  TStackDouble = class(TGStack)
  end;

implementation

{$INCLUDE '..\Generic\StackImplementation.tpl'}


end.
