{$IFDEF INTERFACE}

{$DEFINE TGListIndex := TGQueueIndex}
{$DEFINE TGListItem := TGQueueItem}
{$DEFINE TGList := TGQueueList}
{$DEFINE TGListSortCompare := TGQueueSortCompare}
{$DEFINE TGListStringConverter := TGQueueStringConverter}
{$DEFINE INTERFACE}
{$INCLUDE 'GenericList.inc'}

  // TGQueue<TSetIndex, TSetItem> = class(TGList)
  TGQueue = class
  private
    FList: TGList;
  public
    procedure Enqueue(Value: TGQueueItem);
    function Dequeue: TGQueueItem;
    function Peek: TGQueueItem;
    constructor Create;
    destructor Destroy; override;
    property List: TGList read FList;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION_USES}

  {$DEFINE IMPLEMENTATION_USES}
  {$INCLUDE '..\Generic\GenericList.inc'}

{$UNDEF IMPLEMENTATION_USES}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$DEFINE TGListIndex := TGQueueIndex}
{$DEFINE TGListItem := TGQueueItem}
{$DEFINE TGList := TGQueueList}
{$DEFINE TGListSortCompare := TGQueueSortCompare}
{$DEFINE TGListStringConverter := TGQueueStringConverter}
{$DEFINE IMPLEMENTATION}
{$INCLUDE 'GenericList.inc'}

{ TGQueue }

procedure TGQueue.Enqueue(Value: TGQueueItem);
begin
  FList.Add(Value);
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

{$UNDEF IMPLEMENTATION}
{$ENDIF}