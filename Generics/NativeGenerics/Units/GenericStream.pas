unit GenericStream;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericList;

type
  TGStream<TItem> = class
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
    procedure Assign(Source: TGStream<TItem>); virtual;
    procedure Write(Item: TItem); virtual; abstract;
    procedure WriteArray(Item: array of TItem); virtual; abstract;
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


implementation


procedure TGStream<TItem>.Assign(Source: TGStream<TItem>);
begin
end;

procedure TGStream<TItem>.SetPosition(AValue: TIndex);
begin
  Seek(AValue, soBeginning);
end;

function TGStream<TItem>.GetPosition: TIndex;
begin
  Result := Seek(0, soCurrent);
end;

procedure TGStream<TItem>.SetSize(AValue: TIndex);
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

function TGStream<TItem>.GetSize: TIndex;
var
  OldPosition: Integer;
begin
  OldPosition := Position;
  Result := Seek(0, soEnd);
  Position := OldPosition;
end;

constructor TGStream<TItem>.Create;
begin
  inherited;
end;

end.
