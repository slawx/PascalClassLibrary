{$INCLUDE 'ListImplementation.tpl'}

{ TGStack }

procedure TGStack.Push(Value: TStackItem);
begin
  FList.Add(Value);
end;

function TGStack.Pop: TStackItem;
begin
  Result := FList.Extract(FList.Last);
end;

constructor TGStack.Create;
begin
  FList := TGList.Create;
end;

destructor TGStack.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

