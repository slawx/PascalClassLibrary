unit StackInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TStackIndex = Integer;
  TStackItem = Integer;
{$INCLUDE '..\Generic\StackInterface.tpl'}

type
  TStackInteger = class(TGStack)
  end;

implementation

{$INCLUDE '..\Generic\StackImplementation.tpl'}


end.
