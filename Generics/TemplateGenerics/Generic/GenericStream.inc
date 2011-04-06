{$IFDEF INTERFACE}

TGStreamDataEvent = procedure (Item: TGStreamItem) of object;

// TGStream<TStreamItem> = class
TGStream = class
  FOnData: TGStreamDataEvent;
  procedure Write(Item: TStreamItem);
  property OnData: TGStreamDataEvent read FOnData write FOnData;
end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure TGStream.Write(Item: TGStreamItem);
begin
  if Assigned(FOnData) then
    FOnData(Item);
end;


{$UNDEF IMPLEMENTATION}
{$ENDIF}
