unit UDynNumber;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UBitStream, Math;

type

  { TDynamicNumber }

  TDynamicNumber = class
    Stream: TBitStream;
    procedure WriteNumber(Value: QWord);
    function ReadNumber: QWord;
    constructor Create;
    destructor Destroy; override;
  private
    function ReadNumber2: QWord;
  end;

implementation

{ TDynamicNumber }

procedure TDynamicNumber.WriteNumber(Value: QWord);
var
  Length: Integer;
begin
  Length := Floor(Log2(Value)) + 1;
  if Length > 1 then begin
    Stream.WriteNumber(1, 1);
    WriteNumber(Length - 2);
  end else Stream.WriteNumber(0, 1);
  if Length > 1 then Length := Length - 1;
  Stream.WriteNumber(Value, Length);
end;

function TDynamicNumber.ReadNumber: QWord;
var
  Bit: Byte;
  Length: Integer;
begin
  Length := 0;
  Bit := Stream.ReadNumber(1);
  if Bit = 0 then Length := 1
    else Length := ReadNumber2 + 2;
  if Length > 1 then Result := Stream.ReadNumber(Length - 1)
    else Result := Stream.ReadNumber(Length);
  if Length > 1 then Result := Result or (QWord(1) shl (Length - 1));
end;

function TDynamicNumber.ReadNumber2: QWord;
var
  Bit: Byte;
  Length: Integer;
begin
  Length := 0;
  Bit := Stream.ReadNumber(1);
  if Bit = 0 then Length := 1
    else Length := ReadNumber + 2;
  if Length > 1 then Result := Stream.ReadNumber(Length - 1)
    else Result := Stream.ReadNumber(Length);
  if Length > 1 then Result := Result or (QWord(1) shl (Length - 1));
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
