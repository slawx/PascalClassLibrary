unit ListDouble;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TListIndex = Integer;
  TListItem = Double;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericList.inc'}

type
  TListDouble = class(TGList)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericList.inc'}

end.
