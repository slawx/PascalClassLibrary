unit UDynNumber;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UBitStream;

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
  C: Integer;
  Parts: array of Integer;
begin
  C := 0;
  for C := 0 to Value do
    Stream.WriteBit(True);
  Stream.WriteBit(False);
end;

function TDynamicNumber.Read: QWord;
begin

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
