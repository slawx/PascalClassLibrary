unit UIntelHexFile;

interface

uses
  Classes, UTextFileStream, SysUtils, Contnrs, UCommon, UMemoryStreamEx,
  DateUtils;

type
  EFileCorrupted = class(Exception);
  EChecksumError = class(Exception);

  TIntelHexRecordType = (rtData, rtEndOfFile, rtSegmentAddress, rtExtendedAddress);

  TMappedMemory = class(TMemoryStreamEx)
    BaseAddress: Integer;
  end;

  { TIntelHexFile }

  TIntelHexFile = class
  private
    function GetSize:Integer;
    procedure WriteBlock(Block: TMappedMemory; StringList:TStringList);
    function TwoComplement(Value: Byte): Byte;
  public
    Blocks: TObjectList; // of TMappedMemory
    BytePerLine: Byte;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure LoadFromStringList(StringList:TStringList);
    procedure SaveToStringList(StringList:TStringList);
    procedure LoadFromBinFile(FileName: string);
    procedure SaveToBinFile(FileName: string);
    constructor Create;
    destructor Destroy; override;
    property Size: Integer read GetSize;
  end;


implementation

{ TIntelHexFile }

function TIntelHexFile.GetSize:Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Blocks.Count - 1 do
    Result := Result + TMappedMemory(Blocks[I]).Size;
end;

procedure TIntelHexFile.WriteBlock(Block:TMappedMemory; StringList:TStringList)
  ;
var
  I: Integer;
  L: Integer;
  CheckSum: Byte;
  OutputLine: string;
  Count: Integer;
  Line: TMemoryStreamEx;
begin
  Line := TMemoryStreamEx.Create;
  Block.Position := 0;
  for L := 0 to (Block.Size div BytePerLine) do begin
    Count := BytePerLine;
    if Count > (Block.Size - Block.Position) then
      Count := Block.Size - Block.Position;
    if Count > 0 then begin
      Line.Size := 4 + Count;
      Line.Position := 0;
      Line.WriteByte(Count);
      Line.WriteByte(((Block.Position + Block.BaseAddress) shr 8) and $ff);
      Line.WriteByte((Block.Position + Block.BaseAddress) and $ff);
      Line.WriteByte(Integer(rtData));
      Line.WriteStreamPart(TStream(Block), Count);

      CheckSum := TwoComplement(Line.Sum);
      Line.WriteByte(CheckSum);

      OutputLine := ':';
      Line.Position := 0;
      for I := 0 to Line.Size - 1 do
        OutputLine := OutputLine + IntToHex(Line.ReadByte, 2);
      StringList.Add(OutputLine);
    end;
  end;
  Line.Destroy;
end;

function TIntelHexFile.TwoComplement(Value:Byte):Byte;
begin
  Result := (not Value + 1) and $ff;
end;

procedure TIntelHexFile.LoadFromFile(FileName: string);
var
  StringList: TStringList;
begin
     StringList := TStringList.Create;
  StringList.LoadFromFile(UTF8Decode(FileName));
  LoadFromStringList(StringList);
  StringList.Destroy;
end;

procedure TIntelHexFile.LoadFromStringList(StringList: TStringList);
var
  Row: string;
  DataCount: Byte;
  RecordType: TIntelHexRecordType;
  I: Integer;
  CheckSum: Byte;
  LastAddress: Integer;
  Address: Integer;
  SegmentAddress: Word;
  ExtendedAddress: Word;
  Block: TMappedMemory;
  Line: TMemoryStreamEx;
begin
  Line := TMemoryStreamEx.Create;
  Blocks.Clear;
  Block := nil;
  LastAddress := -1;
  ExtendedAddress := 0;
  SegmentAddress := 0;
  for I := 0 to StringList.Count - 1 do begin
    Row := Trim(StringList[I]);
    if SplitString(Row, 1) <> ':' then
      raise EFileCorrupted.Create('File corrupted');

    Line.Position := 0;
    Line.Size := Length(Row) div 2;
    while Length(Row) > 0 do
      Line.WriteByte(StrToInt('$' + SplitString(Row, 2)));

    // Read CheckSum
    Line.Position := Line.Size - 1;
    CheckSum := Line.ReadByte;
    Line.Size := Line.Size - 1;
    if CheckSum <> TwoComplement(Line.Sum) then
        raise EChecksumError.Create('Checksum error ' + Row);

    Line.Position := 0;
    DataCount := Line.ReadByte;
    Address := ((Line.ReadByte shl 8) or Line.ReadByte) +
      + (SegmentAddress shl 4) + (ExtendedAddress shl 16);

    if LastAddress <> Address then begin
      Block := TMappedMemory.Create;
      Block.BaseAddress := Address;
      Blocks.Add(Block);
    end;
    RecordType := TIntelHexRecordType(Line.ReadByte);
    case RecordType of
      rtData: begin
        Block.WriteStreamPart(Line, Line.Size - Line.Position);
      end;
      rtEndOfFile: begin
        Break;
      end;
      rtSegmentAddress: begin
        SegmentAddress := (Line.ReadByte shl 8) or Line.ReadByte;
        ExtendedAddress := 0;
      end;
      rtExtendedAddress: begin
        ExtendedAddress := (Line.ReadByte shl 8) or Line.ReadByte;
        SegmentAddress := 0;
      end;
    end;
    LastAddress := Address + DataCount;
  end;
  Line.Destroy;
end;

procedure TIntelHexFile.SaveToFile(FileName: string);
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  SaveToStringList(StringList);
  StringList.SaveToFile(UTF8Decode(FileName));
  StringList.Destroy;
end;

procedure TIntelHexFile.SaveToStringList(StringList: TStringList);
var
  I: Integer;
  Line: TMemoryStreamEx;
  OutputLine: string;
  CheckSum: Byte;
begin
  StringList.Clear;

  for I := 0 to Blocks.Count - 1 do
    WriteBlock(TMappedMemory(Blocks[I]), StringList);

  Line := TMemoryStreamEx.Create;

  // Write EndOfFile
  Line.Size := 4;
  Line.WriteByte(0);
  Line.WriteByte(0);
  Line.WriteByte(0);
  Line.WriteByte(Integer(rtEndOfFile));

  CheckSum := TwoComplement(Line.Sum);

  Line.Position := 0;
  OutputLine := ':';
  for I := 0 to Line.Size - 1 do
    OutputLine := OutputLine + IntToHex(Line.ReadByte, 2);
  OutputLine := OutputLine + IntToHex(CheckSum, 2);
  StringList.Add(OutputLine);

  Line.Destroy;
end;

procedure TIntelHexFile.LoadFromBinFile(FileName:string);
var
  NewBlock: TMappedMemory;
begin
  // TODO: analyze empty areas with FF bytes and split them to blocks
  Blocks.Clear;
  NewBlock := TMappedMemory.Create;
  NewBlock.LoadFromFile(UTF8Decode(FileName));
end;

procedure TIntelHexFile.SaveToBinFile(FileName:string);
var
  MergedMemory: TMemoryStreamEx;
  I: Integer;
begin
  MergedMemory := TMemoryStreamEx.Create;

  // Fill unused space by unused data (FF)
  for I := 0 to Blocks.Count - 1 do
   if MergedMemory.Size < (TMappedMemory(Blocks[I]).BaseAddress + TMappedMemory(Blocks[I]).Size) then
     MergedMemory.Size := (TMappedMemory(Blocks[I]).BaseAddress + TMappedMemory(Blocks[I]).Size);
  MergedMemory.FillByte($ff, MergedMemory.Size);

  // Write all memory blocks
  for I := 0 to Blocks.Count - 1 do begin
    MergedMemory.Position := TMappedMemory(Blocks[I]).BaseAddress;
    MergedMemory.WriteStream(TMappedMemory(Blocks[I]), TMappedMemory(Blocks[I]).Size);
  end;
  MergedMemory.SaveToFile(UTF8Decode(FileName));
  MergedMemory.Destroy;
end;

constructor TIntelHexFile.Create;
begin
  Blocks := TObjectList.Create;
  BytePerLine := 16;
end;

destructor TIntelHexFile.Destroy;
begin
  Blocks.Destroy;
  inherited Destroy;
end;

end.
