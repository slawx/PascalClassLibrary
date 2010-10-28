unit StackPointer;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TStackIndex = Integer;
  TStackItem = Pointer;
{$INCLUDE '..\Generic\StackInterface.tpl'}

type
  TStackPointer = class(TGStack)
  end;

implementation

{$INCLUDE '..\Generic\StackImplementation.tpl'}


end.
