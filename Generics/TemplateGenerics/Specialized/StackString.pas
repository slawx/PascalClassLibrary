unit StackString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TStackIndex = Integer;
  TStackItem = string;
{$INCLUDE '..\Generic\StackInterface.tpl'}

type
  TStackString = class(TGStack)
  end;

implementation

{$INCLUDE '..\Generic\StackImplementation.tpl'}

end.
