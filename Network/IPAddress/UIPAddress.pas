unit UIPAddress;

// Date: 2010-05-14

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TIPAddress }

  TIPAddress = class
    Address: Cardinal;
    Prefix: Byte;
    function GetNetMask: Cardinal;
    function GetString: string;
    procedure SetString(Value: string);
    constructor Create;
    procedure Assign(Source: TObject);
    property AsString: string read GetString write SetString;
  end;

implementation

{ TIPAddress }

function TIPAddress.GetNetMask: Cardinal;
begin
  Result := Cardinal((1 shl (32 - Prefix)) - 1) xor $ffffffff;
end;

function TIPAddress.GetString: string;
begin
  Result := IntToStr((Address shr 24) and $ff) + '.' + IntToStr((Address shr 16) and $ff) + '.' +
    IntToStr((Address shr 8) and $ff) + '.' + IntToStr((Address shr 0) and $ff);
  if(Prefix <> 32) then Result := Result + '/' + IntToStr(Prefix);
end;

procedure TIPAddress.SetString(Value: string);
var
  Part: string;
  Octet: Integer;
  TempValue: Integer;
begin
  if Pos('/', Value) > 0 then begin
    if TryStrToInt(Copy(Value, Pos('/', Value) + 1, Length(Value)), TempValue) then
      Prefix := TempValue else Prefix := 32;
    Delete(Value, Pos('/', Value), Length(Value));
  end else Prefix := 32;
  if Pos('.', Value) > 0 then begin
    Part := Copy(Value, 1, Pos('.', Value) - 1);
    Delete(Value, 1, Pos('.', Value));
    if TryStrToInt(Part, TempValue) then Octet := TempValue
      else Octet := 0;
    Address := Address or Cardinal((Octet and $ff) shl 24);
  end;
  if Pos('.', Value) > 0 then begin
    Part := Copy(Value, 1, Pos('.', Value) - 1);
    Delete(Value, 1, Pos('.', Value));
    if TryStrToInt(Part, TempValue) then Octet := TempValue
      else Octet := 0;
    Address := Address or Cardinal((Octet and $ff) shl 16);
  end;
  if Pos('.', Value) > 0 then begin
    Part := Copy(Value, 1, Pos('.', Value) - 1);
    Delete(Value, 1, Pos('.', Value));
    if TryStrToInt(Part, TempValue) then Octet := TempValue
      else Octet := 0;
    Address := Address or Cardinal((Octet and $ff) shl 8);
  end;
  if TryStrToInt(Value, TempValue) then Octet := TempValue
    else Octet := 0;
  Address := Address or Cardinal((Octet and $ff) shl 0);
end;

constructor TIPAddress.Create;
begin
  Address := 0;
  Prefix := 32;
end;

procedure TIPAddress.Assign(Source: TObject);
begin
  if Source is TIPAddress then begin
    Address := TIPAddress(Source).Address;
    Prefix := TIPAddress(Source).Prefix;
  end;
end;

end.

