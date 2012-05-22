unit UIntelHexFile;

interface

uses
  Classes, SysUtils, Contnrs, UCommon, UStreamHelper,
  DateUtils;

type
  EFileCorrupted = class(Exception);
  EChecksumError = class(Exception);

  TIntelHexRecordType = (rtData, rtEndOfFile, rtSegmentAddress, rtExtendedAddress);

  { TMappedMemory }

  TMappedMemory = class(TStreamHelper)
    BaseAddress: Integer;
    procedure Assign(Source: TMappedMemory);
  end;

  { TIntelHexFile }

  TIntelHexFile = class
  private
    function GetSize:Integer;
    procedure WriteBlock(Block: TMappedMemory; StringList:TStringList);
    function TwoComplement(Value: Byte): Byte;
  public
    Blocks: TObjectList; // TListObject<TMappedMemory>
    BytePerLine: Byte;
    procedure Assign(Source: TIntelHexFile);
    function EqualTo(Source: TIntelHexFile): Boolean;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure LoadFromStringList(StringList:TStringList);
    procedure SaveToStringList(StringList:TStringList);
    procedure LoadFromBinFile(FileName: string);
    procedure SaveToBinFile(FileName: string);
    procedure WriteContinuousBlock(AStream: TStream);
    constructor Create;
    destructor Destroy; override;
    property Size: Integer read GetSize;
  end;


implementation

resourcestring
  SCorruptedFile = 'File corrupted "%s: %s".';
  SChecksumError = 'Checksum error "%s: %s".';

{ TMappedMemory }

procedure TMappedMemory.Assign(Source: TMappedMemory);
var
  Buffer: array[0..10000] of Byte;
  Count: Integer;
begin
  BaseAddress := Source.BaseAddress;
  Endianness := Source.Endianness;
  Source.Stream.Position := 0;
  Stream.Size := 0;
  repeat
    Count := Source.Stream.Read(PByte(Buffer)^, SizeOf(Buffer));
    Stream.Write(PByte(Buffer)^, Count);
  until Source.Stream.Position >= Source.Stream.Size;
end;

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
  Line: TStreamHelper;
begin
  try
    Line := TStreamHelper.Create;
    Block.Position := 0;
    for L := 0 to (Block.Size div BytePerLine) do begin
      Count := BytePerLine;
      if Count > (Block.Size - Block.Position) then
        Count := Block.Size - Block.Position;
      if Count > 0 then begin
        Line.Size := 4 + Int64(Count);
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
  finally
    Line.Free;
  end;
end;

function TIntelHexFile.TwoComplement(Value:Byte):Byte;
begin
  Result := (not Value + 1) and $ff;
end;

procedure TIntelHexFile.Assign(Source: TIntelHexFile);
var
  I: Integer;
  NewBlock: TMappedMemory;
begin
  BytePerLine := Source.BytePerLine;
  Blocks.Clear;
  for I := 0 to Source.Blocks.Count - 1 do begin
    NewBlock := TMappedMemory.Create;
    NewBlock.Assign(TMappedMemory(Source.Blocks[I]));
    Blocks.Add(NewBlock);
  end;
end;

function TIntelHexFile.EqualTo(Source: TIntelHexFile): Boolean;
var
  I: Integer;
  NewBlock: TMappedMemory;
begin
  Result := True;
  if Source.Blocks.Count <> Blocks.Count then begin
    Result := False;
    Exit;
  end;
  for I := 0 to Source.Blocks.Count - 1 do begin
    if not TMappedMemory(Blocks[I]).EqualTo(TMappedMemory(Source.Blocks[I])) then begin
      Result := False;
      Exit;
    end;
  end;
end;

procedure TIntelHexFile.LoadFromFile(FileName: string);
var
  StringList: TStringList;
begin
  try
    StringList := TStringList.Create;
    StringList.LoadFromFile(UTF8Decode(FileName));
    LoadFromStringList(StringList);
  finally
    StringList.Free;
  end;
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
  Line: TStreamHelper;
begin
  try
  Line := TStreamHelper.Create;
  Blocks.Clear;
  Block := nil;
  LastAddress := -1;
  ExtendedAddress := 0;
  SegmentAddress := 0;
  for I := 0 to StringList.Count - 1 do begin
    Row := Trim(StringList[I]);
    if Row = '' then Continue;
    if SplitString(Row, 1) <> ':' then
      raise EFileCorrupted.CreateFmt(SCorruptedFile, [IntToStr(I), StringList[I]]);

    Line.Position := 0;
    Line.Size := Length(Row) div 2;
    while Length(Row) > 0 do
      Line.WriteByte(StrToInt('$' + SplitString(Row, 2)));

    // Read CheckSum
    Line.Position := Line.Size - 1;
    CheckSum := Line.ReadByte;
    Line.Size := Line.Size - 1;
    if CheckSum <> TwoComplement(Line.Sum) then
      raise EChecksumError.CreateFmt(SChecksumError, [IntToStr(I), StringList[I]]);

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
  finally
    Line.Free;
  end;
end;

procedure TIntelHexFile.SaveToFile(FileName: string);
var
  StringList: TStringList;
begin
  try
    StringList := TStringList.Create;
    SaveToStringList(StringList);
    StringList.SaveToFile(UTF8Decode(FileName));
  finally
    StringList.Free;
  end;
end;

procedure TIntelHexFile.SaveToStringList(StringList: TStringList);
var
  I: Integer;
  Line: TStreamHelper;
  OutputLine: string;
  CheckSum: Byte;
begin
  try
    StringList.Clear;

    for I := 0 to Blocks.Count - 1 do
      WriteBlock(TMappedMemory(Blocks[I]), StringList);

    Line := TStreamHelper.Create;

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

  finally
    Line.Free;
  end;
end;

procedure TIntelHexFile.LoadFromBinFile(FileName:string);
var
  NewBlock: TMappedMemory;
begin
  // TODO: analyze empty areas with FF bytes and split them to blocks
  Blocks.Clear;
  NewBlock := TMappedMemory.Create(TMemoryStream.Create);
  (NewBlock.Stream as TMemoryStream).LoadFromFile(UTF8Decode(FileName));
  NewBlock.BaseAddress := 0;
  Blocks.Add(NewBlock);
end;

procedure TIntelHexFile.SaveToBinFile(FileName:string);
var
  MergedMemory: TStreamHelper;
  I: Integer;
begin
  try
    MergedMemory := TStreamHelper.Create;

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
    (MergedMemory.Stream as TMemoryStream).SaveToFile(UTF8Decode(FileName));
  finally
    MergedMemory.Free;
  end;
end;

procedure TIntelHexFile.WriteContinuousBlock(AStream: TStream);
var
  I: Integer;
begin
  AStream.Size := 0;
  for I := 0 to Blocks.Count - 1 do
  with TMappedMemory(Blocks[I]) do begin
    Position := 0;
    AStream.Position := BaseAddress;
    ReadStreamPart(AStream, Size);
  end;
end;

constructor TIntelHexFile.Create;
begin
  Blocks := TObjectList.Create;
  BytePerLine := 16;
end;

destructor TIntelHexFile.Destroy;
begin
  Blocks.Free;
  inherited Destroy;
end;

end.
