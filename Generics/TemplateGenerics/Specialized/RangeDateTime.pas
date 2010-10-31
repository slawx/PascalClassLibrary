unit RangeDateTime;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TRangeItem = TDateTime;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericRange.inc'}

type
  TRangeDateTime = class(TGRange)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericRange.inc'}


end.
