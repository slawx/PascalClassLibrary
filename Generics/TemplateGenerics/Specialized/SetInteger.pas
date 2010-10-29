unit SetInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TSetIndex = Integer;
  TSetItem = Integer;
{$INCLUDE '..\Generic\SetInterface.tpl'}

type
  TSetInteger = class(TGSet)
  end;

implementation

{$INCLUDE '..\Generic\SetImplementation.tpl'}


end.
