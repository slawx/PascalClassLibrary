unit GenericStream;

{$mode delphi}

interface

type
  TSeekOrigin = (soBegin, soCurrent, soEnd);

  TGStream<T> = class
  public
    type
      TItemArray = array of T;
  private
    procedure SetSize(AValue: Integer);
    function GetSize: Integer;
    procedure SetPosition(AValue: Integer);
    function GetPosition: Integer;
  public
    procedure Assign(Source: TGStream<T>); virtual;
    procedure Write(Item: T); virtual; abstract;
    procedure WriteArray(Item: array of T); virtual; abstract;
    procedure WriteStream(Stream: TGStream<T>; Count: Integer); virtual; abstract;
    function Read: T; virtual; abstract;
    function ReadArray(Count: Integer): TItemArray; virtual; abstract;
    function ReadStream(Stream: TGStream<T>; Count: Integer): Integer; virtual; abstract;
    function Insert(Count: Integer): Integer; virtual; abstract;
    function Remove(Count: Integer): Integer; virtual; abstract;
    function Seek(Offset: Integer; Origin: TSeekOrigin = soCurrent):
      Integer; virtual; abstract;
    constructor Create; virtual;
    property Position: Integer read GetPosition write SetPosition;
    property Size: Integer read GetSize write SetSize;
  end;


implementation

procedure TGStream<T>.Assign(Source: TGStream<T>);
begin
end;

procedure TGStream<T>.SetPosition(AValue: Integer);
begin
  Seek(AValue, soBegin);
end;

function TGStream<T>.GetPosition: Integer;
begin
  Result := Seek(0, soCurrent);
end;

procedure TGStream<T>.SetSize(AValue: Integer);
var
  StreamSize: Integer;
  OldPosition: Integer;
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

function TGStream<T>.GetSize: Integer;
var
  OldPosition: Integer;
begin
  OldPosition := Position;
  Result := Seek(0, soEnd);
  Position := OldPosition;
end;

constructor TGStream<T>.Create;
begin
  inherited;
end;

end.
