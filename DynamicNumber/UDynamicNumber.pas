unit UDynamicNumber;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { TDynamicNumber }

  TDynamicNumber = class
  private
    function GetSize: Integer;
    procedure SetSize(const AValue: Integer);
    procedure CheckSize(Source: TDynamicNumber);
  public
    Value: array of Byte;
    procedure Assign(AValue: Integer); overload;
    procedure Assign(AValue: Byte); overload;

    function BitwiseAnd(AValue: TDynamicNumber): TDynamicNumber;
    function BitwiseOr(AValue: TDynamicNumber): TDynamicNumber;
    function BitwiseXor(AValue: TDynamicNumber): TDynamicNumber;
    function BitwiseNot: TDynamicNumber;

    function Add(AValue: TDynamicNumber): TDynamicNumber;
    function Subtract(AValue: TDynamicNumber): TDynamicNumber;
    function Divide(AValue: TDynamicNumber): TDynamicNumber;
    function Multiply(AValue: TDynamicNumber): TDynamicNumber;

    function EqualTo(AValue: TDynamicNumber): Boolean;
    function HigerThen(AValue: TDynamicNumber): Boolean;
    function LowerThen(AValue: TDynamicNumber): Boolean;

    procedure Shift(Direction: Integer);
    property Size: Integer read GetSize write SetSize;
  end;

implementation

{ TDynamicNumber }

function TDynamicNumber.GetSize: Integer;
begin
  Result := Length(Value);
end;

procedure TDynamicNumber.SetSize(const AValue: Integer);
begin
  SetLength(Value, AValue);
end;

procedure TDynamicNumber.CheckSize(Source: TDynamicNumber);
begin
  if Source.Size <> Size then
    raise Exception.Create('Size mismatch.');
end;

procedure TDynamicNumber.Assign(AValue: Integer);
begin
  Size := 4;
  Value[0] := AValue;
  Value[1] := AValue shr 8;
  Value[2] := AValue shr 16;
  Value[3] := AValue shr 24;
end;

procedure TDynamicNumber.Assign(aValue: Byte);
begin
  Size := 1;
  Value[0] := AValue;
end;

function TDynamicNumber.BitwiseAnd(AValue: TDynamicNumber): TDynamicNumber;
var
  I: Integer;
begin
  CheckSize(AValue);
  for I := 0 to Length(Value) - 1 do
    Value[I] := Value[I] and AValue.Value[I];
  Result := Self;
end;

function TDynamicNumber.BitwiseOr(AValue: TDynamicNumber): TDynamicNumber;
var
  I: Integer;
begin
  CheckSize(AValue);
  for I := 0 to Length(Value) - 1 do
    Value[I] := Value[I] or AValue.Value[I];
  Result := Self;
end;

function TDynamicNumber.BitwiseXor(AValue: TDynamicNumber): TDynamicNumber;
var
  I: Integer;
begin
  CheckSize(AValue);
  for I := 0 to Length(Value) - 1 do
    Value[I] := Value[I] xor AValue.Value[I];
  Result := Self;
end;

function TDynamicNumber.BitwiseNot: TDynamicNumber;
var
  I: Integer;
begin
  for I := 0 to Length(Value) - 1 do
    Value[I] := not Value[I];
  Result := Self;
end;

function TDynamicNumber.Add(AValue: TDynamicNumber): TDynamicNumber;
begin
  raise Exception.Create('Not implemented');
end;

function TDynamicNumber.Subtract(AValue: TDynamicNumber): TDynamicNumber;
begin
  raise Exception.Create('Not implemented');
end;

function TDynamicNumber.Divide(AValue: TDynamicNumber): TDynamicNumber;
begin
  raise Exception.Create('Not implemented');
end;

function TDynamicNumber.Multiply(AValue: TDynamicNumber): TDynamicNumber;
begin
  raise Exception.Create('Not implemented');
end;

function TDynamicNumber.EqualTo(AValue: TDynamicNumber): Boolean;
var
  I: Integer;
begin
  CheckSize(AValue);
  Result := True;
  for I := 0 to Length(Value) - 1 do
    if Value[I] <> Value[I] then begin
      Result := False;
      Break;
    end;
end;

function TDynamicNumber.HigerThen(AValue: TDynamicNumber): Boolean;
begin
  raise Exception.Create('Not implemented');
end;

function TDynamicNumber.LowerThen(AValue: TDynamicNumber): Boolean;
begin
  raise Exception.Create('Not implemented');
end;

procedure TDynamicNumber.Shift(Direction: Integer);
begin
  raise Exception.Create('Not implemented');
end;


end.

