unit ListString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = string;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericList.inc'}

type

  { TListString }

  TListString = class(TGList)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericList.inc'}

end.
