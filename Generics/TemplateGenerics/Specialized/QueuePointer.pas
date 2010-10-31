unit QueuePointer;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TQueueIndex = Integer;
  TQueueItem = Pointer;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericQueue.inc'}

type
  TQueuePointer = class(TGQueue)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericQueue.inc'}

end.
