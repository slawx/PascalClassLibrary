unit StackString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TStackIndex = Integer;
  TStackItem = string;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericStack.inc'}

type
  TStackString = class(TGStack)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericStack.inc'}

end.
