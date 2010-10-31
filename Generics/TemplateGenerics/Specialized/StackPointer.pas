unit StackPointer;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TStackIndex = Integer;
  TStackItem = Pointer;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericStack.inc'}

type
  TStackPointer = class(TGStack)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericStack.inc'}


end.
