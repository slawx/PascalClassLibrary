{$INCLUDE 'ListImplementation.tpl'}

{ TGStack }

procedure TGStack.Push(Value: TStackItem);
begin
  Add(Value);
end;

function TGStack.Pop: TStackItem;
begin
  Result := Extract(Last);
end;
