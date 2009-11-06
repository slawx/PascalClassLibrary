unit UIntelHexFile;

interface

uses
  UTextFileStream, SysUtils;

type
  TArrayofByte = array of Byte;

  TIntelHexFile = class(TTextFileStream)
  public
    function ReadContent: TArrayOfByte;
  end;


implementation

{ TIntelHexFile }

function TIntelHexFile.ReadContent: TArrayOfByte;
var
  Row: string;
  DataCount: Byte;
  RecordType: Byte;
  I: Integer;
  CRC: Byte;
  ComputedCRC: Byte;
  Address: Word;
  SegmentAddress: Word;
  ExtendedAddress: Word;
  EndOfFile: Boolean;

function SplitString(var Text: string; Count: Word): string;
begin
  Result := Copy(Text, 1, Count);
  Delete(Text, 1, Count);
end;

begin
  ExtendedAddress := 0;
  SegmentAddress := 0;
  EndOfFile := False;
  repeat
    ComputedCRC := 0;
    Row := ReadLn;
    if SplitString(Row, 1) <> ':' then raise Exception.Create('File corrupted');
    for I := 0 to (Length(Row) div 2) - 2 do
      ComputedCRC := ComputedCRC + StrToInt('$' + Row[I * 2 + 1] + Row[I * 2 + 2]);
    ComputedCRC := not (ComputedCRC ) + 1;

    DataCount := StrToInt('$' + SplitString(Row, 2));
    Address := StrToInt('$' + SplitString(Row, 4)) + (SegmentAddress shl 4) +
      (ExtendedAddress shl 16);
    RecordType := StrToInt('$' + SplitString(Row, 2));
    case RecordType of
      0: begin // Data
        if Length(Result) < (Address + DataCount) then
          SetLength(Result, Address + DataCount);
        for I := 0 to DataCount - 1 do begin
          Result[Address + I] := StrToInt('$' + SplitString(Row, 2));
        end;
      end;
      1: begin // End-of-file
        EndOfFile := True;
      end;
      2: begin // Segment address
        SegmentAddress := StrToInt('$' + SplitString(Row, 4));
        ExtendedAddress := 0;
      end;
      4: begin // Linear extended address
        ExtendedAddress := StrToInt('$' + SplitString(Row, 4));
        SegmentAddress := 0;
      end;
    end;
    CRC := StrToInt('$' + SplitString(Row, 2));
    if CRC <> ComputedCRC then raise Exception.Create('Checksum error');
  until Eof or EndOfFile;
end;

end.
