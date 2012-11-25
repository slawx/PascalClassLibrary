unit bgraribbon;

{$mode objfpc}{$H+}

interface

uses
  Controls, Graphics, BGRABitmap, BGRABitmapTypes, BGRAButton;

type
  TBGRARibbonStyle = (rsTab, rsButton, rsSeparator, rsMenu);

{ StyleButtons }

procedure StyleRibbonButtonsSample(AControl: TControl; AStyle: TBGRARibbonStyle);
procedure StyleRibbonBody(AControl: TControl);

{ Drawings }

procedure DrawTabGradient(ABitmap: TBGRABitmap);
procedure DrawBodyGradient(ABitmap: TBGRABitmap);
procedure DrawFormGradient(ABitmap: TBGRABitmap);

{ Buttons }

procedure RibbonTab(AButton: TBGRAButton);
procedure RibbonButton(AButton: TBGRAButton);
procedure RibbonSeparator(AButton: TBGRAButton);
procedure RibbonMenu(AButton: TBGRAButton);

implementation

{ StyleButtons }

procedure StyleButtons(AControl: TControl; AButton: TBGRAButton);
var
  i: integer;
  WinControl: TWinControl;
begin
  if AControl is TBGRAButton then
    AControl.Assign(AButton);
  if AControl is TWinControl then
  begin
    WinControl := TWinControl(AControl);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
      StyleButtons(WinControl.Controls[i], AButton);
  end;
end;

procedure StyleRibbonButtonsSample(AControl: TControl; AStyle: TBGRARibbonStyle);
var
  tempBGRAButton: TBGRAButton;
begin
  tempBGRAButton := TBGRAButton.Create(nil);
  case AStyle of
    rsTab: RibbonTab(tempBGRAButton);
    rsButton: RibbonButton(tempBGRAButton);
    rsSeparator: RibbonSeparator(tempBGRAButton);
    rsMenu: RibbonMenu(tempBGRAButton);
  end;
  StyleButtons(AControl, tempBGRAButton);
  tempBGRAButton.Free;
end;

procedure StyleRibbonBody(AControl: TControl);
var
  i: integer;
  WinControl: TWinControl;
begin
  if AControl is TWinControl then
  begin
    WinControl := TWinControl(AControl);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
    begin
      if WinControl.Controls[i] is TBGRAButton then
      begin
        if WinControl.Controls[i].Caption = '-' then
        begin
          StyleRibbonButtonsSample(WinControl.Controls[i], rsSeparator);
          WinControl.Controls[i].Caption := '';
        end
        else
          StyleRibbonButtonsSample(WinControl.Controls[i], rsButton);
      end;
    end;
  end;
end;

{ Drawings }

procedure DrawTabGradient(ABitmap: TBGRABitmap);
begin
  with ABitmap do
  begin
    Fill(BGRA(223, 233, 245, 255));
    DrawHorizLine(0, Height - 1, Width - 1, BGRA(186, 201, 219, 255));
  end;
end;

procedure DrawBodyGradient(ABitmap: TBGRABitmap);
begin
  with ABitmap do
  begin
    GradientFill(0, 0, Width, Height, BGRA(247, 251, 255, 255),
      BGRA(220, 231, 245, 255), gtLinear, PointF(0, 0), PointF(0, Height - 3), dmSet);
    Rectangle(0, 0, Width, Height - 2, BGRA(255, 255, 255, 75), BGRAPixelTransparent,
      dmDrawWithTransparency);
    DrawHorizLine(0, Height - 2, Width - 1, BGRA(206, 219, 235, 255));
    DrawHorizLine(0, Height - 1, Width - 1, BGRA(159, 174, 194, 255));
  end;
end;

procedure DrawFormGradient(ABitmap: TBGRABitmap);
begin
  with ABitmap do
  begin
    GradientFill(0, 0, Width, Height, BGRA(197, 207, 223, 255),
      BGRA(220, 229, 242, 255), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
  end;
end;

{ Buttons }

procedure RibbonTab(AButton: TBGRAButton);
begin
  with AButton do
  begin
    TextCanvas := True;
    TextShadow := False;
    RoundX := 1;
    RoundY := 1;
    with BodyNormal do
    begin
      Font.Color := RGBToColor(30, 57, 91);
      Color := clWhite;
      BorderColor := RGBToColor(186, 201, 219);
      Style := bbsColor;
    end;
    with BodyHover do
    begin
      Font.Color := RGBToColor(30, 57, 91);
      Color := clWhite;
      BorderColor := RGBToColor(186, 201, 219);
      Style := bbsColor;
    end;
    with BodyClicked do
    begin
      Font.Color := RGBToColor(30, 57, 91);
      Color := clWhite;
      BorderColor := RGBToColor(186, 201, 219);
      Style := bbsColor;
    end;
  end;
end;

procedure RibbonButton(AButton: TBGRAButton);
begin
  with AButton do
  begin
    TextCanvas := True;
    TextShadow := False;
    RoundX := 2;
    RoundY := 2;
    with BodyNormal do
    begin
      Font.Color := RGBToColor(30, 57, 91);
      BorderStyle := bboNone;
      Style := bbsClear;
    end;
    with BodyHover do
    begin
      Font.Color  := RGBToColor(30, 57, 91);
      BorderColor := RGBToColor(255, 183, 0);
      Gradient1.StartColor := RGBToColor(253, 236, 218);
      Gradient1.EndColor := RGBToColor(253, 223, 186);
      Gradient2.StartColor := RGBToColor(255, 206, 105);
      Gradient2.EndColor := RGBToColor(255, 255, 217);
      Gradient2.Point1YPercent := 60;
      Gradient2.Point2YPercent := 100;
      LightOpacity := 200;
      LightWidth  := 1;
    end;
    with BodyClicked do
    begin
      Font.Color  := RGBToColor(30, 57, 91);
      BorderColor := RGBToColor(194, 155, 41);
      Gradient1.StartColor := RGBToColor(241, 206, 146);
      Gradient1.EndColor := RGBToColor(245, 199, 119);
      Gradient2.StartColor := RGBToColor(245, 187, 86);
      Gradient2.EndColor := RGBToColor(242, 242, 177);
      Gradient2.Point1YPercent := 60;
      Gradient2.Point2YPercent := 100;
    end;
  end;
end;

procedure RibbonSeparator(AButton: TBGRAButton);
begin
  with AButton do
  begin
    StaticButton := True;
    with BodyNormal do
    begin
      BorderColor := clWhite;
      BorderColorOpacity := 150;
      Color := RGBToColor(165, 184, 208);
      Style := bbsColor;
    end;
  end;
end;

procedure RibbonMenu(AButton: TBGRAButton);
begin
  with AButton do
  begin
    TextCanvas := True;
    TextShadowOffSetX := 1;
    TextShadowOffSetY := 1;
    TextShadowRadius  := 0;
    with BodyNormal do
    begin
      Font.Color  := clWhite;
      BorderColor := RGBToColor(65, 106, 189);
      Gradient1.StartColor := RGBToColor(70, 129, 190);
      Gradient1.EndColor := RGBToColor(41, 92, 170);
      Gradient2.StartColor := RGBToColor(26, 65, 136);
      Gradient2.EndColor := RGBToColor(66, 154, 207);
      LightOpacity := 100;
      LightWidth  := 1;
      Gradient1EndPercent := 50;
      Gradient1.Point1YPercent := 30;
      Gradient2.Point1YPercent := 30;
    end;
    with BodyHover do
    begin
      Font.Color  := clWhite;
      BorderColor := RGBToColor(68, 135, 213);
      Gradient1.StartColor := RGBToColor(123, 177, 235);
      Gradient1.EndColor := RGBToColor(71, 128, 207);
      Gradient2.StartColor := RGBToColor(17, 78, 175);
      Gradient2.EndColor := RGBToColor(128, 255, 255);
      LightOpacity := 100;
      LightWidth  := 1;
      Gradient1EndPercent := 50;
      Gradient1.Point1YPercent := 30;
      Gradient2.Point1YPercent := 30;
    end;
    with BodyClicked do
    begin
      Font.Color  := clWhite;
      BorderStyle := bboNone;
      Gradient1.StartColor := RGBToColor(75, 138, 197);
      Gradient1.EndColor := RGBToColor(49, 104, 176);
      Gradient2.StartColor := RGBToColor(28, 67, 138);
      Gradient2.EndColor := RGBToColor(53, 139, 201);
      Gradient1EndPercent := 50;
      Gradient1.Point1YPercent := 30;
      Gradient2.Point1YPercent := 30;
    end;
  end;
end;

end.
