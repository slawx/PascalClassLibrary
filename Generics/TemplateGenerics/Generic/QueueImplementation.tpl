{$INCLUDE 'ListImplementation.tpl'}

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

