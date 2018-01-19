{$IFDEF INTERFACE}

{$DEFINE TGListIndex := TGQueueIndex}
{$DEFINE TGListItem := TGQueueItem}
{$DEFINE TGList := TGQueueList}
{$DEFINE INTERFACE}
{$I 'GenericList.inc'}

  // TGQueue<TQueueIndex, TQueueItem> = class(TGList)
  TGQueue = class
  private
    FList: TGList;
    function GetCount: TGQueueIndex;
  public
    procedure Enqueue(Value: TGQueueItem);
    procedure EnqueueArray(Values: array of TGQueueItem);
    procedure EnqueueList(List: TGList);
    function Dequeue: TGQueueItem;
    function Peek: TGQueueItem;
    constructor Create;
    destructor Destroy; override;
    property List: TGList read FList;
    property Count: TGQueueIndex read GetCount;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION_USES}

  {$DEFINE IMPLEMENTATION_USES}
  {$I 'GenericList.inc'}

{$UNDEF IMPLEMENTATION_USES}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$DEFINE TGListIndex := TGQueueIndex}
{$DEFINE TGListItem := TGQueueItem}
{$DEFINE TGList := TGQueueList}
{$DEFINE IMPLEMENTATION}
{$I 'GenericList.inc'}

{ TGQueue }

procedure TGQueue.Enqueue(Value: TGQueueItem);
begin
  FList.Add(Value);
end;

procedure TGQueue.EnqueueArray(Values: array of TGQueueItem);
begin
  FList.AddArray(Values);
end;

procedure TGQueue.EnqueueList(List: TGList);
begin
  FList.AddList(List);
end;

function TGQueue.Peek: TGQueueItem;
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

function TGQueue.Dequeue: TGQueueItem;
begin
  Result := FList.Extract(FList.First);
end;

function TGQueue.GetCount: TGQueueIndex;
begin
  Result := FList.Count;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}
