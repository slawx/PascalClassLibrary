unit UIPv4Edit;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, StrUtils;

type

{ TIPv4Edit }

  TIPv4Edit = class(TEdit)
  private
    FAddress: Cardinal;
    procedure ChangeExecute(Sender: TObject);
    procedure SetAddress(const AValue: Cardinal);
  public
    function TryStrToIPv4(S: string; out Value: Cardinal): Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Address: Cardinal read FAddress write SetAddress;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TEditExtensions', [TIPv4Edit]);
end;

{ TIPv4Edit }

procedure TIPv4Edit.ChangeExecute(Sender: TObject);
var
  NewText: string;
  Temp: Cardinal;
begin
  NewText := Text;
  Delete(NewText, SelStart + 1, 1);
  if TryStrToIPv4(NewText, Temp) then
    FAddress := Temp;
  SetAddress(FAddress);
end;

procedure TIPv4Edit.SetAddress(const AValue: Cardinal);
var
  LastPos: Integer;
  Adr: string;
  I: Integer;
  Octet: string;
begin
  FAddress := AValue;
  LastPos := SelStart;
  OnChange := nil;
  Adr := '';
  for I := 0 to 3 do begin
    Octet := IntToStr((FAddress shr (24 - (I * 8))) and $ff);
    Adr := Adr + Octet + DupeString(' ', 3 - Length(Octet));
    if I < 3 then Adr := Adr + '.';
  end;
  Text := Adr;
  OnChange := ChangeExecute;
  if LastPos = 3 then LastPos := 4;
  if LastPos = 7 then LastPos := 8;
  if LastPos = 11 then LastPos := 12;
  SelStart := LastPos;
end;

function TIPv4Edit.TryStrToIPv4(S: string; out Value: Cardinal): Boolean;
var
  P: Integer;
  Octet: Integer;
  OctetText: string;
  I: Integer;
begin
  Result := False;
  Value := 0;
  for I := 0 to 3 do begin
    if I < 3 then begin
      P := Pos('.', S);
      if P > 0 then
        OctetText := Copy(S, 1, P - 1) else Exit;
      Delete(S, 1, P);
    end else OctetText := S;
    if TryStrToInt(Trim(OctetText), Octet) then begin
      if (Octet >= 0) and (Octet <= 255) then
        Value := Value or (Byte(Octet) shl (24 - (I * 8)))
        else Exit;
    end else Exit;
  end;
  Result := True;
end;

constructor TIPv4Edit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnChange := ChangeExecute;
end;

destructor TIPv4Edit.Destroy;
begin
  inherited Destroy;
end;

end.

