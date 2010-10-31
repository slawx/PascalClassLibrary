unit QueueInteger;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TQueueIndex = Integer;
  TQueueItem = Integer;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericQueue.inc'}

type
  TQueueInteger = class(TGQueue)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericQueue.inc'}

end.
