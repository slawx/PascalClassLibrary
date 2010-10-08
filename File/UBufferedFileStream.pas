unit UBufferedFileStream;

interface

uses
  Classes;

type
  TBufferedFileStream = class(TStream)
  private
    FFile: TFileStream;
    FBuffer: TMemoryStream;
    FBufferPosition: Integer;
    FBufferMaxSize: Integer;
    FPosition: Integer;
    FSize: Integer;
    FDoFlush: Boolean;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
    function GetBufferUsed: Integer;
    function GetSize: Int64; override;
  public
    constructor Create(const AFileName: string; Mode: Word); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure Flush;
    property BufferMaxSize: Integer read FBufferMaxSize write FBufferMaxSize;
    property BufferUsed: Integer read GetBufferUsed;
  end;

implementation

{ TBufferedFileStream }

constructor TBufferedFileStream.Create(const AFileName: string; Mode: Word;
  Rights: Cardinal);
begin
  FFile := TFileStream.Create(AFileName, Mode, Rights);
  FSize := FFile.Size;
  FBuffer := TMemoryStream.Create;
  FBufferMaxSize := 60000;
  FPosition := 0;
end;

constructor TBufferedFileStream.Create(const AFileName: string;
  Mode: Word);
begin
  {$IFDEF MSWINDOWS}
    Create(AFilename, Mode, 0);
  {$ELSE}
    Create(AFilename, Mode, FileAccessRights);
  {$ENDIF}
end;

destructor TBufferedFileStream.Destroy;
begin
  Flush;
  FFile.Free;
  FBuffer.Free;
  inherited;
end;

procedure TBufferedFileStream.Flush;
var
  BufferMemory: Pointer;
  WritedCount: Integer;
begin
  if FDoFlush then begin
    // Write buffer to disk
    FFile.Position := FBufferPosition;
    BufferMemory := FBuffer.Memory;
    FBuffer.Position := 0;
    WritedCount := FFile.Write(BufferMemory^, FBuffer.Size);
    FBuffer.Size := 0;
    FDoFlush := False;
    FBufferPosition := FBufferPosition + WritedCount;
  end;
end;

function TBufferedFileStream.GetBufferUsed: Integer;
begin
  Result := FBuffer.Size;
end;

function TBufferedFileStream.GetSize: Int64;
begin
  Result := FSize;
end;

function TBufferedFileStream.Read(var Buffer; Count: Integer): Longint;
var
  ReadedCount: Integer;
  TotalReadedCount: Integer;
  BufferMemory: Pointer;
  BufferPointer: Pointer;
  RequestRead: Integer;
begin
  TotalReadedCount := 0;
  repeat
    BufferPointer := Pointer(Integer(Addr(Buffer)) + TotalReadedCount);
    RequestRead := Count - TotalReadedCount;
    ReadedCount := FBuffer.Read(BufferPointer^, RequestRead);
    Inc(FPosition, ReadedCount);
    Inc(TotalReadedCount, ReadedCount);
    if ReadedCount < RequestRead then begin
      if FDoFlush then Flush;
      // Not enough data in memory buffer, read next
      FBufferPosition := FFile.Position;
      FBuffer.Size := FBufferMaxSize; // Allocate max. buffer size
      BufferMemory := FBuffer.Memory;
      FBuffer.Position := 0;
      ReadedCount := FFile.Read(BufferMemory^, FBufferMaxSize); // Try to load max amount of data
      FBuffer.Size := ReadedCount; // Lower buffer size
    end;
  until (TotalReadedCount >= Count) or (Position = Size);
  Result := TotalReadedCount;
end;

function TBufferedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FFile.Size + Offset;
  end;
  if (FPosition > (FBufferPosition + FBuffer.Size)) or (FPosition < FBufferPosition) then begin
    FFile.Seek(FPosition, soFromBeginning);
    Flush;
    FBuffer.Clear;
  end;

  Result := FPosition;
end;

procedure TBufferedFileStream.SetSize(NewSize: Integer);
begin
  FBuffer.Size := 0;
  inherited;
end;

procedure TBufferedFileStream.SetSize(const NewSize: Int64);
begin
  inherited;
  FBuffer.Size := 0;
end;

function TBufferedFileStream.Write(const Buffer; Count: Integer): Longint;
var
  WritedCount: Integer;
  TotalWritedCount: Integer;
  BufferMemory: Pointer;
  BufferPointer: Pointer;
  RequestWrite: Integer;
begin
  TotalWritedCount := 0;
  repeat
    BufferPointer := Pointer(Integer(Addr(Buffer)) + TotalWritedCount);
    RequestWrite := Count - TotalWritedCount;
    if RequestWrite > (FBufferMaxSize - FBuffer.Position) then // Limit max buffer size
      WritedCount := FBufferMaxSize - FBuffer.Position
      else WritedCount := RequestWrite;

    WritedCount := FBuffer.Write(BufferPointer^, WritedCount);
    Inc(FPosition, WritedCount);
    if FPosition > FSize then FSize := FPosition;    
    Inc(TotalWritedCount, WritedCount);
    FDoFlush := True;

    if (WritedCount < RequestWrite) and FDoFlush then Flush;
  until (TotalWritedCount >= Count);
  Result := TotalWritedCount;
end;

end.
