unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UBitStream;

type
  TArrayOfByte = array of Byte;

  { TMainForm }

  TMainForm = class(TForm)
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
  private
    procedure PrintBitStream(Stream: TBitStream; StringList: TStrings);
    procedure PrintData(Data: TArrayOfByte; Count: Integer; StringList: TStrings);
    procedure PrintStream(Stream: TStream; StringList: TStrings);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormShow(Sender: TObject);
var
  BitStream: TMemoryBitStream;
  BitStream2: TMemoryBitStream;
  Buffer: array of Byte;
  I: Integer;
  Y: Integer;
  C: Integer;
begin
  WindowState := wsMaximized;

  with Memo1, Lines do try
    BeginUpdate;
    BitStream := TMemoryBitStream.Create;
    BitStream2 := TMemoryBitStream.Create;
    SetLength(Buffer, 4);
    Buffer[0] := $12;
    Buffer[1] := $34;
    Buffer[2] := $56;
    Buffer[3] := $78;
    Add('Source data:');
    PrintData(Buffer, Length(Buffer) * 8, Lines);

    BitStream.Write(Buffer[0], 28);
    Add('Write data to stream:');
    PrintBitStream(BitStream, Lines);
    // Write second bit array after first which lead to store data as shifted
    BitStream.Write(Buffer[0], 28);
    Add('Append shifted data to stream:');
    PrintBitStream(BitStream, Lines);

    // Read test of sub bit array
    BitStream.Position := 1;
    BitStream.Read(Buffer[0], 32);
    Add('Read bit data part:');
    PrintData(Buffer, Length(Buffer) * 8, Lines);

   (* // Test stream copy
    Add('Copy bit block to another stream:');
    for I := 0 to BitStream.Size do begin
      BitStream.Position := I;
      BitStream2.Size := 0;
      BitStream2.CopyFrom(BitStream, BitStream.Size);
      PrintBitStream(BitStream2, Lines);
    end;
    for I := 0 to BitStream.Size do begin
      BitStream.Position := 0;
      BitStream2.Size := 0;
      BitStream2.Position := I;
      BitStream2.CopyFrom(BitStream, BitStream.Size);
      PrintBitStream(BitStream2, Lines);
    end;  *)

    Add('Bit copy of substreams');
    SetLength(Buffer, 1000);
    for Y := 1 to BitStream.Size do begin
      Add('Size: ' + IntToStr(Y));
      for I := 0 to BitStream.Size - Y do begin
        BitStream.Position := I;
        BitStream2.Size := 0;
        //BitStream2.CopyFrom(BitStream, Y);
        //PrintBitStream(BitStream2, Lines);
        SetLength(Buffer, 1000);
        C := BitStream.Read(Buffer[0], Y);
        SetLength(Buffer, C div 8 + 1);
        PrintData(Buffer, C, Lines);
      end;
      Add('');
    end;
  finally
    BitStream.Free;
    BitStream2.Free;
    EndUpdate;
  end;
end;

procedure TMainForm.PrintData(Data: TArrayOfByte; Count: Integer; StringList: TStrings);
var
  I: Integer;
  B: Integer;
  OneByte: Byte;
  Line: string;
begin
  Text := '';
  if Length(Data) > 0 then
  for I := 0 to High(Data) do begin
    OneByte := Data[I];
    for B := 0 to 7 do begin
      if ((I * 8) + B) >= Count then Break;
      Line := Line + IntToStr((OneByte shr B) and 1);
    end;
  end;
  StringList.Add(Line);
  //StringList.Add('');
end;

procedure TMainForm.PrintStream(Stream: TStream; StringList: TStrings);
var
  I: Integer;
  B: Integer;
  Data: Byte;
  Line: string;
begin
  Line := '';
  Stream.Position := 0;
  for I := 0 to Stream.Size - 1 do begin
    Data := Stream.ReadByte;
    for B := 0 to 7 do
      Line := Line + IntToStr((Data shr B) and 1);
  end;
  StringList.Add(Line);
  StringList.Add('');
end;

procedure TMainForm.PrintBitStream(Stream: TBitStream; StringList: TStrings);
begin
  StringList.Add(Stream.AsString);
end;

end.

