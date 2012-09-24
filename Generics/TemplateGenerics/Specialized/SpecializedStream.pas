unit SpecializedStream;

{$mode delphi}

interface

uses
  Classes, SysUtils, SpecializedList, DateUtils;

type
  TSeekOrigin = (soBegin, soCurrent, soEnd);

{$MACRO ON}

// TStreamInteger<Integer, Integer>
{$DEFINE TGStreamIndex := Integer}
{$DEFINE TGStreamItem := Integer}
{$DEFINE TGStreamList := TListStreamInteger}
{$DEFINE TGStream := TStreamInteger}
{$DEFINE INTERFACE}
{$I 'GenericStream.inc'}

// TStreamByte<Integer, Byte>
{$DEFINE TGStreamIndex := Integer}
{$DEFINE TGStreamItem := Byte}
{$DEFINE TGStreamList := TListStreamByte}
{$DEFINE TGStream := TBaseStreamByte}
{$DEFINE INTERFACE}
{$I 'GenericStream.inc'}

// TStreamPointer<Integer, Pointer>
{$DEFINE TGStreamIndex := Integer}
{$DEFINE TGStreamItem := Pointer}
{$DEFINE TGStreamList := TListStreamPointer}
{$DEFINE TGStream := TStreamPointer}
{$DEFINE INTERFACE}
{$I 'GenericStream.inc'}

  TStreamByte = class(TBaseStreamByte)
    function ReadBuffer(var Buffer; Count: Integer): Integer; virtual; abstract;
    function WriteBuffer(var Buffer; Count: Integer): Integer; virtual; abstract;
  end;

  { TMemoryStreamByte }

  TMemoryStreamByte = class(TStreamByte)
  private
    FList: TListByte;
    FOwnsList: Boolean;
    FPosition: Integer;
  public
    procedure Assign(Source: TBaseStreamByte); override;
    procedure Write(Item: Byte); override;
    procedure WriteArray(Values: array of Byte); override;
    procedure WriteList(List: TListByte);
    function WriteBuffer(var Buffer; Count: Integer): Integer; override;
    procedure WriteStream(Stream: TBaseStreamByte; Count: Integer); override;
    function Read: Byte; override;
    function ReadArray(Count: Integer): TItemArray; override;
    function ReadList(List: TListByte; Count: Integer): Integer;
    function ReadBuffer(var Buffer; Count: Integer): Integer; override;
    function ReadStream(Stream: TBaseStreamByte; Count: Integer): Integer; override;
    function Insert(Count: Integer): Integer; override;
    function Remove(Count: Integer): Integer; override;
    function Seek(Offset: Integer; Origin: TSeekOrigin = soCurrent): Integer; override;
    constructor Create; override; overload;
    constructor Create(AList: TListByte); overload;
    destructor Destroy; override;
    property OwnsList: Boolean read FOwnsList write FOwnsList;
    property List: TListByte read FList;
  end;


implementation

{ TMemoryStreamByte }

procedure TMemoryStreamByte.Assign(Source: TBaseStreamByte);
begin
  inherited;
  if Source is TMemoryStreamByte then begin
    FList.Assign(TMemoryStreamByte(Source).FList);
    FPosition := TMemoryStreamByte(Source).FPosition;
  end;
end;

procedure TMemoryStreamByte.Write(Item: Byte);
begin
  if FList.Count < (FPosition + 1) then
    FList.Count := FPosition + 1;
  FList[FPosition] := Item;
  Inc(FPosition);
end;

procedure TMemoryStreamByte.WriteArray(Values: array of Byte);
begin
  if FList.Count < (FPosition + Length(Values)) then
    FList.Count := FPosition + Length(Values);
  FList.ReplaceArray(FPosition, Values);
  Inc(FPosition, Length(Values));
end;

procedure TMemoryStreamByte.WriteList(List: TListByte);
begin
  FList.ReplaceList(FPosition, List);
end;

procedure TMemoryStreamByte.WriteStream(Stream: TBaseStreamByte; Count: Integer);
begin

end;

function TMemoryStreamByte.WriteBuffer(var Buffer; Count: Integer): Integer;
begin

end;

function TMemoryStreamByte.Read: Byte;
begin
  Result := FList[FPosition];
  Inc(FPosition);
end;

function TMemoryStreamByte.ReadArray(Count: Integer): TItemArray;
begin
  Result := FList.GetArray(FPosition, Count);
end;

function TMemoryStreamByte.ReadList(List: TListByte; Count: Integer): Integer;
begin
  if (FPosition + Count) > FList.Count then
    Count := FList.Count - FPosition;
  FList.GetList(List, FPosition, Count);
  Result := Count;
end;

function TMemoryStreamByte.ReadBuffer(var Buffer; Count: Integer): Integer;
begin

end;

function TMemoryStreamByte.ReadStream(Stream: TBaseStreamByte; Count: Integer
  ): Integer;
begin

end;

function TMemoryStreamByte.Insert(Count: Integer): Integer;
begin
  FList.InsertCount(FPosition, Count);
  Result := Count;
end;

function TMemoryStreamByte.Remove(Count: Integer): Integer;
begin
  Result := FList.Count - FPosition;
  if Count < Result then Result := Count;
  FList.DeleteItems(FPosition, Count);
end;

function TMemoryStreamByte.Seek(Offset: Integer; Origin: TSeekOrigin): Integer;
begin
  case Origin of
    soBegin: FPosition := Offset;
    soCurrent: FPosition := FPosition + Offset;
    soEnd: FPosition := FList.Count + Offset;
  end;
  if FPosition > FList.Count then FPosition := FList.Count;
  if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

constructor TMemoryStreamByte.Create;
begin
  inherited;
  FList := TListByte.Create;
  OwnsList := True;
end;

constructor TMemoryStreamByte.Create(AList: TListByte);
begin
  inherited Create;
  FList := AList;
  OwnsList := False;
end;

destructor TMemoryStreamByte.Destroy;
begin
  if OwnsList then FList.Free;
  inherited Destroy;
end;

{$DEFINE IMPLEMENTATION_USES}
{$I 'GenericStream.inc'}

// TStreamInteger<Integer, Integer>
{$DEFINE TGStreamIndex := Integer}
{$DEFINE TGStreamItem := Integer}
{$DEFINE TGStreamList := TListStreamInteger}
{$DEFINE TGStream := TStreamInteger}
{$DEFINE IMPLEMENTATION}
{$I 'GenericStream.inc'}

// TStreamByte<Integer, Byte>
{$DEFINE TGStreamIndex := Integer}
{$DEFINE TGStreamItem := Byte}
{$DEFINE TGStreamList := TListStreamByte}
{$DEFINE TGStream := TBaseStreamByte}
{$DEFINE IMPLEMENTATION}
{$I 'GenericStream.inc'}

// TStreamPointer<Integer, Pointer>
{$DEFINE TGStreamIndex := Integer}
{$DEFINE TGStreamItem := Pointer}
{$DEFINE TGStreamList := TListStreamPointer}
{$DEFINE TGStream := TStreamPointer}
{$DEFINE IMPLEMENTATION}
{$I 'GenericStream.inc'}



end.
