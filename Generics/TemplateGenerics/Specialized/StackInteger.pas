unit StackInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TStackIndex = Integer;
  TStackItem = Integer;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericStack.inc'}

type
  TStackInteger = class(TGStack)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericStack.inc'}


end.
