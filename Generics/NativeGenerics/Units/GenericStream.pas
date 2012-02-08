unit GenericStream;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericList;

type
  TGAbstractStream<TItem> = class
  public
    type
      TIndex = NativeInt;
      TItemArray = array of TItem;
      TSeekOrigin = (soBeginning, soCurrent, soEnd);
  private
    procedure SetSize(AValue: TIndex);
    function GetSize: TIndex;
    procedure SetPosition(AValue: TIndex);
    function GetPosition: TIndex;
  public
    procedure Assign(Source: TGAbstractStream<TItem>); virtual;
    procedure Write(Item: TItem); virtual; abstract;
    procedure WriteArray(Item: array of TItem); virtual; abstract;
    procedure WriteList(List: TGList<TItem>); virtual; abstract;
    function Read: TItem; virtual; abstract;
    function ReadArray(Count: TIndex): TItemArray; virtual; abstract;
    function Insert(Count: TIndex): TIndex; virtual; abstract;
    function Remove(Count: TIndex): TIndex; virtual; abstract;
    function Seek(Offset: TIndex; Origin: TSeekOrigin = soCurrent):
      TIndex; virtual; abstract;
    constructor Create; virtual;
    property Position: TIndex read GetPosition write SetPosition;
    property Size: TIndex read GetSize write SetSize;
  end;

  TGStream<TItem> = class(TGAbstractStream<TItem>)
  private
    FList: TGList<TItem>;
    FPosition: TIndex;
  public
    procedure Assign(Source: TGAbstractStream<TItem>); override;
    procedure Write(Item: TItem); override;
    procedure WriteArray(Values: array of TItem); override;
    procedure WriteList(List: TGList<TItem>); override;
    function Read: TItem; override;
    function ReadArray(Count: TIndex): TItemArray; override;
    function ReadList(List: TGList<TItem>; Count: TIndex): TIndex;
    function Insert(Count: TIndex): Integer; override;
    function Remove(Count: TIndex): Integer; override;
    function Seek(Offset: TIndex; Origin: TSeekOrigin = soCurrent): TIndex; override;
    constructor Create; override;
    destructor Destroy; override;
    property List: TGList<TItem> read FList;
  end;


implementation


{ TGStream }

procedure TGAbstractStream<TItem>.Assign(Source: TGAbstractStream<TItem>);
begin
end;

procedure TGAbstractStream<TItem>.SetPosition(AValue: TIndex);
begin
  Seek(AValue, soBeginning);
end;

function TGAbstractStream<TItem>.GetPosition: TIndex;
begin
  Result := Seek(0, soCurrent);
end;

procedure TGAbstractStream<TItem>.SetSize(AValue: TIndex);
var
  StreamSize: TIndex;
  OldPosition: TIndex;
begin
  OldPosition := Seek(0, soCurrent);
  StreamSize := Size;
  if AValue > StreamSize then begin
    Seek(StreamSize, soBeginning);
    Insert(AValue - StreamSize);
  end else
  if AValue < StreamSize then begin
    Seek(AValue, soBeginning);
    Remove(StreamSize - AValue);
  end;
  Position := OldPosition;
end;

function TGAbstractStream<TItem>.GetSize: TIndex;
var
  OldPosition: Integer;
begin
  OldPosition := Position;
  Result := Seek(0, soEnd);
  Position := OldPosition;
end;

constructor TGAbstractStream<TItem>.Create;
begin
  inherited;
end;

{ TGStream }

procedure TGStream<TItem>.Assign(Source: TGAbstractStream<TItem>);
begin
  inherited;
  if Source is TGStream<TItem> then begin
    FList.Assign(TGStream<TItem>(Source).FList);
    FPosition := TGStream<TItem>(Source).FPosition;
  end;
end;

procedure TGStream<TItem>.Write(Item: TItem);
begin
  if FList.Count < (FPosition + 1) then
    FList.Count := FPosition + 1;
  FList[FPosition] := Item;
  Inc(FPosition);
end;

procedure TGStream<TItem>.WriteArray(Values: array of TItem);
begin
  if FList.Count < (FPosition + Length(Values)) then
    FList.Count := FPosition + Length(Values);
  FList.ReplaceArray(FPosition, Values);
  Inc(FPosition, Length(Values));
end;

procedure TGStream<TItem>.WriteList(List: TGList<TItem>);
begin
  FList.ReplaceList(FPosition, List);
end;

function TGStream<TItem>.Read: TItem;
begin
  Result := FList[FPosition];
  Inc(FPosition);
end;

function TGStream<TItem>.ReadArray(Count: TIndex): TItemArray;
begin
  Result := FList.GetArray(FPosition, Count);
end;

function TGStream<TItem>.ReadList(List: TGList<TItem>; Count: TIndex): TIndex;
begin
  if (FPosition + Count) > FList.Count then
    Count := FList.Count - FPosition;
  FList.GetList(List, FPosition, Count);
  Result := Count;
end;

function TGStream<TItem>.Insert(Count: TIndex): TIndex;
begin
  FList.InsertCount(FPosition, Count);
  Result := Count;
end;

function TGStream<TItem>.Remove(Count: TIndex): TIndex;
begin
  Result := FList.Count - FPosition;
  if Count < Result then Result := Count;
  FList.DeleteItems(FPosition, Count);
end;

function TGStream<TItem>.Seek(Offset: TIndex; Origin: TSeekOrigin): TIndex;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: FPosition := FPosition + Offset;
    soEnd: FPosition := FList.Count + Offset;
  end;
  if FPosition > FList.Count then FPosition := FList.Count;
  if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

constructor TGStream<TItem>.Create;
begin
  inherited;
  FList := TGList<TItem>.Create;
end;

destructor TGStream<TItem>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;


end.
