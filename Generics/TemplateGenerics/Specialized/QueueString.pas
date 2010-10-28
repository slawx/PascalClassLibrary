unit QueueString;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TQueueIndex = Integer;
  TQueueItem = string;
{$INCLUDE '..\Generic\QueueInterface.tpl'}

type
  TQueueString = class(TGQueue)
  end;

implementation

{$INCLUDE '..\Generic\QueueImplementation.tpl'}

end.
