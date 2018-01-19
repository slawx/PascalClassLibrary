{$IFDEF INTERFACE}


// TGStream<TGStreamIndex, TGStreamItem> = class
TGStream = class
public
  type
    TItemArray = array of TGStreamItem;
private
procedure SetSize(AValue: TGStreamIndex);
  function GetSize: TGStreamIndex;
  procedure SetPosition(AValue: TGStreamIndex);
  function GetPosition: TGStreamIndex;
public
  procedure Assign(Source: TGStream); virtual;
  procedure Write(Item: TGStreamItem); virtual; abstract;
  procedure WriteArray(Item: array of TGStreamItem); virtual; abstract;
  procedure WriteStream(Stream: TGStream; Count: TGStreamIndex); virtual; abstract;
  function Read: TGStreamItem; virtual; abstract;
  function ReadArray(Count: TGStreamIndex): TItemArray; virtual; abstract;
  function ReadStream(Stream: TGStream; Count: TGStreamIndex): TGStreamIndex; virtual; abstract;
  function Insert(Count: TGStreamIndex): TGStreamIndex; virtual; abstract;
  function Remove(Count: TGStreamIndex): TGStreamIndex; virtual; abstract;
  function Seek(Offset: TGStreamIndex; Origin: TSeekOrigin = soCurrent):
    TGStreamIndex; virtual; abstract;
  constructor Create; virtual;
  property Position: TGStreamIndex read GetPosition write SetPosition;
  property Size: TGStreamIndex read GetSize write SetSize;
end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure TGStream.Assign(Source: TGStream);
begin
end;

procedure TGStream.SetPosition(AValue: TGStreamIndex);
begin
  Seek(AValue, soBegin);
end;

function TGStream.GetPosition: TGStreamIndex;
begin
  Result := Seek(0, soCurrent);
end;

procedure TGStream.SetSize(AValue: TGStreamIndex);
var
  StreamSize: TGStreamIndex;
  OldPosition: TGStreamIndex;
begin
  OldPosition := Seek(0, soCurrent);
  StreamSize := Size;
  if AValue > StreamSize then begin
    Seek(StreamSize, soBegin);
    Insert(AValue - StreamSize);
  end else
  if AValue < StreamSize then begin
    Seek(AValue, soBegin);
    Remove(StreamSize - AValue);
  end;
  Position := OldPosition;
end;

function TGStream.GetSize: TGStreamIndex;
var
  OldPosition: Integer;
begin
  OldPosition := Position;
  Result := Seek(0, soEnd);
  Position := OldPosition;
end;

constructor TGStream.Create;
begin
  inherited;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}
