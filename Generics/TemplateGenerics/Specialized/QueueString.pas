unit QueueString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TQueueIndex = Integer;
  TQueueItem = string;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericQueue.inc'}

type
  TQueueString = class(TGQueue)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericQueue.inc'}

end.
