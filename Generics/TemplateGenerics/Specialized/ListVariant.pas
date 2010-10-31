unit ListVariant;

{$mode delphi}

interface

uses
  Classes, SysUtils, Variants;

type
  TListIndex = Integer;
  TListItem = Variant;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericList.inc'}

type

  { TListVariant }

  TListVariant = class(TGList)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericList.inc'}

end.
