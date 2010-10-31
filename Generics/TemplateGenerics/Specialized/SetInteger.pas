unit SetInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TSetIndex = Integer;
  TSetItem = Integer;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericSet.inc'}

type
  TSetInteger = class(TGSet)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericSet.inc'}


end.
