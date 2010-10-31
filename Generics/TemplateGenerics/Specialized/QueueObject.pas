unit QueueObject;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TQueueIndex = Integer;
  TQueueItem = TObject;
{$DEFINE INTERFACE}
{$INCLUDE '..\Generic\GenericQueue.inc'}

type

  { TObjectQueue }

  TObjectQueue = class(TGQueue)
    //OwnObjects: Boolean;
    destructor Destroy; override;
  end;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE '..\Generic\GenericQueue.inc'}

{ TObjectQueue }

destructor TObjectQueue.Destroy;
begin
  inherited Destroy;
end;

end.
