unit RangeDateTime;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TRangeItem = TDateTime;
{$INCLUDE '..\Generic\RangeInterface.tpl'}

type
  TRangeDateTime = class(TGRange)
  end;

implementation

{$INCLUDE '..\Generic\RangeImplementation.tpl'}


end.
