unit QueueDouble;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TQueueIndex = Integer;
  TQueueItem = Double;
{$INCLUDE '..\Generic\QueueInterface.tpl'}

type
  TQueueDouble = class(TGQueue)
  end;

implementation

{$INCLUDE '..\Generic\QueueImplementation.tpl'}


end.
