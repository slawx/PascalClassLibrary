unit QueuePointer;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TQueueIndex = Integer;
  TQueueItem = Pointer;
{$INCLUDE '..\Generic\QueueInterface.tpl'}

type
  TQueuePointer = class(TGQueue)
  end;

implementation

{$INCLUDE '..\Generic\QueueImplementation.tpl'}


end.
