unit UDynNumber;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UBitStream, Math;

type

  { TDynamicNumber }

  TDynamicNumber = class
    Stream: TBitStream;
    procedure Write(Value: QWord);
    function Read: QWord;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TDynamicNumber }

procedure TDynamicNumber.Write(Value: QWord);
var
  Bit: Byte;
  Length: Integer;
begin
  Length := Floor(Log2(Value)) + 1;
  if Length > 1 then begin
    Stream.WriteNumber(1, 1);
    Write(Length - 2);
  end else Stream.WriteNumber(0, 1);
  if Length > 1 then Length := Length - 1;
  Stream.WriteNumber(Value, Length);
end;

function TDynamicNumber.Read: QWord;
var
  Bit: Byte;
  Length: Integer;
begin
  Bit := Stream.ReadNumber(1);
  if Bit = 0 then Length := 1
    else Length := Read + 2;
  Result := Stream.ReadNumber(Length);
  if Length > 0 then Result := Result or (1 shl Length);
end;

constructor TDynamicNumber.Create;
begin
  Stream := TMemoryBitStream.Create;
end;

destructor TDynamicNumber.Destroy;
begin
  Stream.Free;
  inherited Destroy;
end;

end.
