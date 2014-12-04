unit UScaleDPI;

{ See: http://wiki.lazarus.freepascal.org/High_DPI }

{$mode delphi}{$H+}

interface

uses
  Classes, Forms, Graphics, Controls, ComCtrls, LCLType;

type

  { TScaleDPI }

  TScaleDPI = class(TComponent)
  private
    FAutoDetect: Boolean;
    procedure SetAutoDetect(AValue: Boolean);
  public
    DPI: TPoint;
    DesignDPI: TPoint;
    procedure ApplyToAll(FromDPI: TPoint);
    procedure ScaleDPI(Control: TControl; FromDPI: TPoint);
    procedure ScaleImageList(ImgList: TImageList; FromDPI: TPoint);
    function ScaleXY(Size: TPoint; FromDPI: Integer): TPoint;
    function ScaleX(Size: Integer; FromDPI: Integer): Integer;
    function ScaleY(Size: Integer; FromDPI: Integer): Integer;
    constructor Create(AOwner: TComponent);
  published
    property AutoDetect: Boolean read FAutoDetect write SetAutoDetect;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Common', [TScaleDPI]);
end;

procedure TScaleDPI.SetAutoDetect(AValue: Boolean);
begin
  if FAutoDetect = AValue then Exit;
  FAutoDetect := AValue;
  if AValue then begin
    DPI := Point(ScreenInfo.PixelsPerInchX, ScreenInfo.PixelsPerInchY);
  end;
end;

procedure TScaleDPI.ApplyToAll(FromDPI: TPoint);
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do begin
    ScaleDPI(Screen.Forms[I], FromDPI);
  end;
end;

procedure TScaleDPI.ScaleImageList(ImgList: TImageList; FromDPI: TPoint);
var
  TempBmp: TBitmap;
  Temp: array of TBitmap;
  NewWidth, NewHeight: integer;
  I: Integer;
begin
  NewWidth := ScaleX(ImgList.Width, FromDPI.X);
  NewHeight := ScaleY(ImgList.Height, FromDPI.Y);

  SetLength(Temp, ImgList.Count);
  TempBmp := TBitmap.Create;
  for I := 0 to ImgList.Count - 1 do
  begin
    ImgList.GetBitmap(I, TempBmp);
    //TempBmp.PixelFormat := pfDevice;
    Temp[I] := TBitmap.Create;
    Temp[I].SetSize(NewWidth, NewHeight);
    Temp[I].TransparentColor := TempBmp.TransparentColor;
    //Temp[I].TransparentMode := TempBmp.TransparentMode;
    Temp[I].Transparent := True;
    Temp[I].Canvas.Brush.Style := bsSolid;
    Temp[I].Canvas.Brush.Color := Temp[I].TransparentColor;
    Temp[I].Canvas.FillRect(0, 0, Temp[I].Width, Temp[I].Height);

    if (Temp[I].Width = 0) or (Temp[I].Height = 0) then Continue;
    Temp[I].Canvas.StretchDraw(Rect(0, 0, Temp[I].Width, Temp[I].Height), TempBmp);
  end;
  TempBmp.Free;

  ImgList.Clear;
  ImgList.Width := NewWidth;
  ImgList.Height := NewHeight;

  for I := 0 to High(Temp) do
  begin
    ImgList.Add(Temp[I], nil);
    Temp[i].Free;
  end;
end;

function TScaleDPI.ScaleX(Size: Integer; FromDPI: Integer): Integer;
begin
  Result := MulDiv(Size, DPI.X, FromDPI);
end;

function TScaleDPI.ScaleY(Size: Integer; FromDPI: Integer): Integer;
begin
  Result := MulDiv(Size, DPI.Y, FromDPI);
end;

function TScaleDPI.ScaleXY(Size: TPoint; FromDPI: Integer): TPoint;
begin
  Result.X := ScaleX(Size.X, FromDPI);
  Result.Y := ScaleY(Size.Y, FromDPI);
end;

constructor TScaleDPI.Create(AOwner: TComponent);
begin
  inherited;
  DPI := Point(96, 96);
  DesignDPI := Point(96, 96);
end;

procedure TScaleDPI.ScaleDPI(Control: TControl; FromDPI: TPoint);
var
  I: Integer;
  WinControl: TWinControl;
  ToolBarControl: TToolBar;
begin
  with Control do begin
    Left := ScaleX(Left, FromDPI.X);
    Top := ScaleY(Top, FromDPI.Y);
    Width := ScaleX(Width, FromDPI.X);
    Height := ScaleY(Height, FromDPI.Y);
    {$IFDEF LCL Qt}
      Font.Size := 0;
    {$ELSE}
      Font.Height := ScaleY(Font.GetTextHeight('Hg'), FromDPI.Y);
    {$ENDIF}
  end;

  if Control is TToolBar then begin
    ToolBarControl := TToolBar(Control);
    with ToolBarControl do begin
      ButtonWidth := ScaleX(ButtonWidth, FromDPI.X);
      ButtonHeight := ScaleY(ButtonHeight, FromDPI.Y);
    end;
  end;

  if Control is TWinControl then begin
    WinControl := TWinControl(Control);
    if WinControl.ControlCount > 0 then begin
      for I := 0 to WinControl.ControlCount - 1 do begin
        if WinControl.Controls[I] is TControl then begin
          ScaleDPI(WinControl.Controls[I], FromDPI);
        end;
      end;
    end;
  end;
end;

end.