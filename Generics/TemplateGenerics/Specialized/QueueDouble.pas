unit QueueDouble;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TQueueIndex = Integer;
  TQueueItem = Double;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericQueue.inc'}

type
  TQueueDouble = class(TGQueue)
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericQueue.inc'}

end.
