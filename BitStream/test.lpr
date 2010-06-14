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
  PrintData(Buffer);

  BitStream.Write(Buffer[0], 27);
  // Write second bit array after first which lead to store data as shifted
  BitStream.Write(Buffer[0], 27);

  BitStream.Stream.Position := 0;
  PrintStream(BitStream.Stream);

  // Read test of sub bit array
  BitStream.Position := 5;
  BitStream.Read(Buffer[0], 18);
  PrintData(Buffer);

  // Test stream copy
  BitStream.Position := 2;
  BitStream2.CopyFrom(BitStream, BitStream.Size);
  PrintStream(BitStream2.Stream);

  BitStream.Destroy;
  BitStream2.Destroy;

  ReadLn;
  Terminate;
end;

procedure TTest.PrintData(Data:TByteArray);
var
  I: Integer;
begin
  for I := 0 to High(Data) do
    Write(IntToHex(Data[I], 2) + ' ');
  WriteLn;
end;

procedure TTest.PrintStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Position := 0;
  for I := 0 to Stream.Size - 1 do
    Write(IntToHex(Stream.ReadByte, 2) + ' ');
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

