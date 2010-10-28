unit QueueInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TQueueIndex = Integer;
  TQueueItem = Integer;
{$INCLUDE '..\Generic\QueueInterface.tpl'}

type
  TQueueInteger = class(TGQueue)
  end;

implementation

{$INCLUDE '..\Generic\QueueImplementation.tpl'}


end.
