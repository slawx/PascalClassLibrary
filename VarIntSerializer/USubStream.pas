unit USubStream;

{$mode delphi}

interface

uses
  Classes, SysUtils, UBase;

type

  { TSubStream }

  TSubStream = class(TStream)
  private
    FSource: TStream;
    FSourcePosition: Int64;
    FSize, FPosition: Longint;
  public
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Assign(ASource: TObject);
    property Source: TStream read FSource write FSource;
    property SourcePosition: Int64 read FSourcePosition write FSourcePosition;
  end;

implementation

{ TSubStream }

function TSubStream.GetSize:Int64;
begin
  Result := FSize;
end;

procedure TSubStream.SetSize(NewSize:Longint);
begin
  FSize := NewSize;
  if FPosition > FSize then
    FPosition := FSize;
end;

function TSubStream.Seek(Offset:Longint;Origin:Word):Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromEnd: FPosition := FSize + Offset;
    soFromCurrent: FPosition := FPosition + Offset;
  end;
  Result := FPosition;
end;

function TSubStream.Read(var Buffer; Count:Longint): Longint;
var
  TempPos: Int64;
begin
  TempPos := FSource.Position;
  FSource.Position := FSourcePosition + Position;
  Result := FSource.Read(Buffer, Count);
  FSource.Position := TempPos;
  Inc(FPosition, Result);
end;

function TSubStream.Write(const Buffer;Count:Longint):Longint;
var
  TempPos: Int64;
begin
  TempPos := FSource.Position;
  FSource.Position := FSourcePosition + Position;
  Result := Source.Write(Buffer, Count);
  FSource.Position := TempPos;
  Inc(FPosition, Result);
end;

procedure TSubStream.Assign(ASource: TObject);
begin
  if ASource is TSubStream then begin
    FSource := TSubStream(ASource).FSource;
    FSourcePosition := TSubStream(ASource).FSourcePosition;
    FSize := TSubStream(ASource).FSize;
    FPosition := TSubStream(ASource).FPosition;
  end;
end;

end.

