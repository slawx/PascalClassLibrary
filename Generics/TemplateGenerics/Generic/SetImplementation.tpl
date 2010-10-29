{$INCLUDE 'ListImplementation.tpl'}

{ TGSet }

function TGSet.IsIn(Item: TSetItem): Boolean;
begin
  Result := FList.IndexOf(Item) <> -1;
end;

constructor TGSet.Create;
begin
  FList := TGList.Create;
end;

destructor TGSet.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;
