unit QueueObject;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TQueueIndex = Integer;
  TQueueItem = TObject;
{$INCLUDE '..\Generic\QueueInterface.tpl'}

type

  { TObjectQueue }

  TObjectQueue = class(TGQueue)
    //OwnObjects: Boolean;
    destructor Destroy; override;
  end;

implementation

{$INCLUDE '..\Generic\QueueImplementation.tpl'}


{ TObjectQueue }

destructor TObjectQueue.Destroy;
begin
  inherited Destroy;
end;

end.
