unit UEditTime;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, DateUtils, Controls, ExtCtrls;

type

  { TTimeEdit }

  TTimeEdit = class(TEdit)
  private
    FTime: TTime;
    procedure SetTime(const AValue: TTime);
    procedure ChangeExecute(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Time: TTime read FTime write SetTime;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TEditExtensions', [TTimeEdit]);
end;

{ TTimeEdit }

procedure TTimeEdit.ChangeExecute(Sender: TObject);
var
  NewText: string;
  Temp: TTime;
begin
  NewText := Text;
  Delete(NewText, SelStart + 1, 1);
  if TryStrToTime(NewText, Temp) then
    Time := Temp;
end;

procedure TTimeEdit.SetTime(const AValue: TTime);
var
  LastPos: Integer;
begin
  if FTime = AValue then Exit;
  FTime := AValue;

  LastPos := SelStart;
  OnChange := nil;
  Text := TimeToStr(FTime);
  if HourOf(FTime) < 10 then Text := '0' + Text;
  OnChange := ChangeExecute;
  if LastPos = 2 then Inc(LastPos);
  if LastPos = 5 then Inc(LastPos);
  SelStart := LastPos;
end;

constructor TTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TTimeEdit.Destroy;
begin
  inherited Destroy;
end;

end.

