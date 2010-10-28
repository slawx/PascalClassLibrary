{$INCLUDE 'ListImplementation.tpl'}

{ TGQueue }

procedure TGQueue.Enqueue(Value: TQueueItem);
begin
  Add(Value);
end;

function TGQueue.Peek: TQueueItem;
begin
  Result := First;
end;

function TGQueue.Dequeue: TQueueItem;
begin
  Result := Extract(First);
end;
