unit StackDouble;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TStackIndex = Integer;
  TStackItem = Double;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericStack.inc'}

type
  TStackDouble = class(TGStack)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericStack.inc'}


end.
