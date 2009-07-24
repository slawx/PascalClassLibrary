unit UMemoryStreamEx;

interface

uses
  Classes, DateUtils;

type
  TMemoryStreamEx = class(TMemoryStream)
    procedure WriteByte(Data: Byte);
    procedure WriteWord(Data: Word);
    procedure WriteCardinal(Data: Cardinal);
    procedure WriteInt64(Data: Int64);
    procedure WriteShortString(Data: ShortString);
    procedure WriteUnixTime(Data: TDateTime);
    procedure WriteStream(Stream: TStream);
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadCardinal: Cardinal;
    function ReadInt64: Int64;
    function ReadShortString: string;
    function ReadUnixTime: TDateTime;
    procedure ReadStream(var Stream: TStream; Count: Integer);
  end;

implementation

{ TMemoryStreamEx }

function TMemoryStreamEx.ReadByte: Byte;
begin
  Read(Result, 1);
end;

function TMemoryStreamEx.ReadCardinal: Cardinal;
begin
  Read(Result, 4);
end;

function TMemoryStreamEx.ReadInt64: Int64;
begin
  Read(Result, 8);
end;

function TMemoryStreamEx.ReadShortString: string;
var
  Count: Byte;
begin
  Read(Count, 1);
  SetLength(Result, Count);
  Read(Result[1], Count);
end;

procedure TMemoryStreamEx.ReadStream(var Stream: TStream; Count: Integer);
var
  Buffer: array of Byte;
begin
  if Count > 0 then begin
    SetLength(Buffer, Count);
    Read(Buffer[0], Count);
    Stream.Size := Count;
    Stream.Position := 0;
    Stream.Write(Buffer[0], Count);
  end;
end;

function TMemoryStreamEx.ReadUnixTime: TDateTime;
begin
  Result := UnixToDateTime(ReadCardinal);
end;

function TMemoryStreamEx.ReadWord: Word;
begin
  Read(Result, 2);
end;

procedure TMemoryStreamEx.WriteByte(Data: Byte);
begin
  Write(Data, 1);
end;

procedure TMemoryStreamEx.WriteCardinal(Data: Cardinal);
begin
  Write(Data, 4);
end;

procedure TMemoryStreamEx.WriteInt64(Data: Int64);
begin
  Write(Data, 8);
end;

procedure TMemoryStreamEx.WriteShortString(Data: ShortString);
begin
  WriteByte(Length(Data));
  Write(Data[1], Length(Data))
end;

procedure TMemoryStreamEx.WriteStream(Stream: TStream);
var
  Buffer: array of Byte;
begin
  Stream.Position := 0;
  SetLength(Buffer, Stream.Size);
  Stream.Read(Buffer[0], Stream.Size);
  Write(Buffer[0], Stream.Size);
end;

procedure TMemoryStreamEx.WriteUnixTime(Data: TDateTime);
var
  DataUnix: Int64;
begin
  DataUnix := DateTimeToUnix(Data);
  Write(DataUnix, 4);
end;

procedure TMemoryStreamEx.WriteWord(Data: Word);
begin
  Write(Data, 2);
end;

end.
