unit SetString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TSetIndex = Integer;
  TSetItem = string;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericSet.inc'}

type

  { TSetString }

  TSetString = class(TGSet)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericSet.inc'}

end.
