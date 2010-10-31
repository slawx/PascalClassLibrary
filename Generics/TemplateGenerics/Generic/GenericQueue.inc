{$IFDEF INTERFACE}

  TListIndex = TQueueIndex;
  TListItem = TQueueItem;
  {$DEFINE INTERFACE}
  {$INCLUDE 'GenericList.inc'}

  // TGQueue<TSetIndex, TSetItem> = class(TGList)
  TGQueue = class
  private
    FList: TGList;
  public
    procedure Enqueue(Value: TQueueItem);
    function Dequeue: TQueueItem;
    function Peek: TQueueItem;
    constructor Create;
    destructor Destroy; override;
    property List: TGList read FList;
  end;
{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$DEFINE IMPLEMENTATION}
{$INCLUDE 'GenericList.inc'}

{ TGQueue }

procedure TGQueue.Enqueue(Value: TQueueItem);
begin
  FList.Add(Value);
end;

function TGQueue.Peek: TQueueItem;
begin
  Result := FList.First;
end;

constructor TGQueue.Create;
begin
  FList := TGList.Create;
end;

destructor TGQueue.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TGQueue.Dequeue: TQueueItem;
begin
  Result := FList.Extract(FList.First);
end;

{$ENDIF}
