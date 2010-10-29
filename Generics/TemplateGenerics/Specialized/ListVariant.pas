unit ListVariant;

{$mode delphi}

interface

uses
  Classes, SysUtils, Variants;

type
  TListIndex = Integer;
  TListItem = Variant;
{$INCLUDE '..\Generic\ListInterface.tpl'}

type

  { TListVariant }

  TListVariant = class(TGList)
  end;

implementation

{$INCLUDE '..\Generic\ListImplementation.tpl'}


end.
