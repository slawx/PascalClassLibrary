program test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  UBitStream;

type
  TByteArray = array of Byte;

  { TTest }

  TTest = class(TCustomApplication)
  private
    procedure PrintBitStream(Stream:TBitStream);
  protected
    procedure DoRun; override;
  public
    procedure PrintData(Data: TByteArray);
    procedure PrintStream(Stream: TStream);
  end;

{ TTest }

procedure TTest.DoRun;
var
  BitStream: TMemoryBitStream;
  BitStream2: TMemoryBitStream;
  Buffer: array of Byte;
  I: Integer;
 begin
  BitStream := TMemoryBitStream.Create;
  BitStream2 := TMemoryBitStream.Create;
  SetLength(Buffer, 4);
  Buffer[0] := $12;
  Buffer[1] := $34;
  Buffer[2] := $56;
  Buffer[3] := $78;
  WriteLn('Source data:');
  PrintData(Buffer);

  BitStream.Write(Buffer[0], 28);
  WriteLn('Write data to stream:');
  PrintBitStream(BitStream);
  // Write second bit array after first which lead to store data as shifted
  BitStream.Write(Buffer[0], 28);
  WriteLn('Append shifted data to stream:');
  PrintBitStream(BitStream);

  // Read test of sub bit array
  BitStream.Position := 1;
  BitStream.Read(Buffer[0], 32);
  WriteLn('Read bit data part:');
  PrintData(Buffer);

  // Test stream copy
  WriteLn('Copy bit block to another stream:');
  for I := 0 to BitStream.Size do begin
    BitStream.Position := I;
    BitStream2.Size := 0;
    BitStream2.CopyFrom(BitStream, BitStream.Size);
    PrintBitStream(BitStream2);
  end;
  for I := 0 to BitStream.Size do begin
    BitStream.Position := 0;
    BitStream2.Size := 0;
    BitStream2.Position := I;
    BitStream2.CopyFrom(BitStream, BitStream.Size);
    PrintBitStream(BitStream2);
  end;

  BitStream.Destroy;
  BitStream2.Destroy;

  ReadLn;
  Terminate;
end;

procedure TTest.PrintData(Data:TByteArray);
var
  I: Integer;
  B: Integer;
  OneByte: Byte;
begin
  for I := 0 to High(Data) do begin
    OneByte := Data[I];
    for B := 0 to 7 do
      Write(IntToStr((OneByte shr B) and 1));
  end;
  WriteLn;
end;

procedure TTest.PrintStream(Stream: TStream);
var
  I: Integer;
  B: Integer;
  Data: Byte;
begin
  Stream.Position := 0;
  for I := 0 to Stream.Size - 1 do begin
    Data := Stream.ReadByte;
    for B := 0 to 7 do
      Write(IntToStr((Data shr B) and 1));
  end;
  WriteLn;
end;

procedure TTest.PrintBitStream(Stream: TBitStream);
var
  I: Integer;
begin
  Stream.Position := 0;
  for I := 0 to Stream.Size - 1 do
    Write(IntToStr(Integer(Stream.ReadBit)));
  WriteLn;
end;

var
  Application: TTest;

{$R *.res}

begin
  Application := TTest.Create(nil);
  Application.Title := 'Test';
  Application.Run;
  Application.Free;
end.

