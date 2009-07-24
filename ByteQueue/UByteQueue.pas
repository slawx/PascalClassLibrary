unit UByteQueue;

interface

uses
  Classes, SysUtils, UMemoryStreamEx;
  
type  
  TByteQueue = class
  private
    First: Integer;
    Last: Integer;
    Data: array of Byte;
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function GetUsed: Integer;
  public
    procedure WriteByte(Value: Byte);
    procedure WriteStream(Stream: TStream);
    function ReadByte: Byte;
    procedure Clear;
    property Size: Integer read GetSize write SetSize;
    property Used: Integer read GetUsed;
  end;


implementation

{ TByteQueue }

function TByteQueue.GetUsed: Integer;
begin
  Result := (Last + Size - First) mod Size;
end;

function TByteQueue.ReadByte: Byte;
begin
  if Used > 0 then begin
    Result := Data[First];
    First := (First + 1) mod Size;
  end else
    raise Exception.Create('Buffer empty');
end;

procedure TByteQueue.Clear;
begin
  First := 0;
  Last := 0;
end;

function TByteQueue.GetSize: Integer;
begin
  Result := Length(Data);
end;

procedure TByteQueue.WriteByte(Value: Byte);
begin
  if Used < Size then begin
    Data[Last] := Value;
    Last := (Last + 1) mod Size;
  end else
    raise Exception.Create('Buffer overflow');
end;

procedure TByteQueue.WriteStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Position := 0;
  for I := 0 to Stream.Size - 1 do
    WriteByte(TMemoryStreamEx(Stream).ReadByte);
end;

procedure TByteQueue.SetSize(const Value: Integer);
begin
  SetLength(Data, Value);
end;

end.
