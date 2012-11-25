{ Customizable component which using BGRABitmap for drawing. Control mostly rendered
  using framework.

  Functionality:
  - Gradients
  - Double gradients
  - Rounding
  - Drop down list
  - Glyph
  - States (normal, hover, clicked)
  - Caption with shadow
  - Full alpha and antialias support

  Copyright (C) 2012 Krzysztof Dibowski dibowski at interia.pl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit BCButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Controls, Dialogs, BGRABitmap, BGRABitmapTypes,
  Buttons, Graphics, LCLType, types, BCTypes, Forms, BCBasectrls;

{off $DEFINE DEBUG}

type

  TBCButtonState = class;
  TBCButtonStyle = (bbtButton, bbtDropDown);
  TOnAfterRenderBCButton = procedure(Sender: TObject; const ABGRA: TBGRABitmap;
    AState: TBCButtonState; ARect: TRect) of object;
  TBCButtonPropertyData = (pdNone, pdUpdateSize);

  { TBCButtonState }

  TBCButtonState = class(TBCProperty)
  private
    FBackground: TBCBackground;
    FBorder: TBCBorder;
    FFontEx: TBCFont;
    procedure OnChangeFont(Sender: TObject; AData: PtrInt);
    procedure OnChangeChildProperty(Sender: TObject; AData: PtrInt);
    procedure SetBackground(AValue: TBCBackground);
    procedure SetBorder(AValue: TBCBorder);
    procedure SetFontEx(const AValue: TBCFont);
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Background: TBCBackground read FBackground write SetBackground;
    property Border: TBCBorder read FBorder write SetBorder;
    property FontEx: TBCFont read FFontEx write SetFontEx;
  end;

  { TCustomBCButton }

  TCustomBCButton = class(TBCStyleGraphicControl)
  private
    { Private declarations }
    {$IFDEF DEBUG}
    FRenderCount: Integer;
    {$ENDIF}
    FDropDownArrowSize: Integer;
    FDropDownWidth: Integer;
    FFlipArrow: boolean;
    FActiveButt: TBCButtonStyle;
    FBGRANormal, FBGRAHover, FBGRAClick: TBGRABitmapEx;
    FRounding: TBCRounding;
    FRoundingDropDown: TBCRounding;
    FStateClicked: TBCButtonState;
    FStateHover: TBCButtonState;
    FStateNormal: TBCButtonState;
    FDown: boolean;
    FGlyph: TBitmap;
    FGlyphMargin: integer;
    FButtonState: TBCMouseState;
    FDownButtonState: TBCMouseState;
    FOnAfterRenderBCButton: TOnAfterRenderBCButton;
    FOnButtonClick: TNotifyEvent;
    FStaticButton: boolean;
    FStyle: TBCButtonStyle;
    FGlobalOpacity: byte;
    FTextApplyGlobalOpacity: boolean;
    AutoSizeExtraY: integer;
    AutoSizeExtraX: integer;
    procedure AssignDefaultStyle;
    procedure CalculateGlyphSize(var NeededWidth, NeededHeight: integer);
    procedure ConvertToGrayScale(ABGRA: TBGRABitmap);
    procedure Render(ABGRA: TBGRABitmapEx; AState: TBCButtonState);
    procedure RenderState(ABGRA: TBGRABitmapEx; AState: TBCButtonState;
      const ARect: TRect; ARounding: TBCRounding);
    procedure RenderAll(ANow: Boolean = False);
    function GetButtonRect: TRect;
    function GetDropDownWidth(AFull: boolean = True): integer;
    function GetDropDownRect(AFull: Boolean = True): TRect;
    procedure SeTBCButtonStateClicked(const AValue: TBCButtonState);
    procedure SeTBCButtonStateHover(const AValue: TBCButtonState);
    procedure SeTBCButtonStateNormal(const AValue: TBCButtonState);
    procedure SetDown(AValue: boolean);
    procedure SetDropDownArrowSize(AValue: Integer);
    procedure SetDropDownWidth(AValue: Integer);
    procedure SetFlipArrow(AValue: boolean);
    procedure SetGlyph(const AValue: TBitmap);
    procedure SetGlyphMargin(const AValue: integer);
    procedure SetRounding(AValue: TBCRounding);
    procedure SetRoundingDropDown(AValue: TBCRounding);
    procedure SetStaticButton(const AValue: boolean);
    procedure SetStyle(const AValue: TBCButtonStyle);
    procedure SetGlobalOpacity(const AValue: byte);
    procedure SetTextApplyGlobalOpacity(const AValue: boolean);
    procedure UpdateSize;
    procedure OnChangeGlyph(Sender: TObject);
    procedure OnChangeState(Sender: TObject; AData: PtrInt);
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
  protected
    {$IFDEF DEBUG}
    function GetDebugText: String; override;
    {$ENDIF}
    function GetStyleExtension: String; override;
    procedure DrawControl; override;
    procedure RenderControl; override;
  protected
    property AutoSizeExtraVertical: integer read AutoSizeExtraY;
    property AutoSizeExtraHorizontal: integer read AutoSizeExtraX;
    property StateNormal: TBCButtonState read FStateNormal write SeTBCButtonStateNormal;
    property StateHover: TBCButtonState read FStateHover write SeTBCButtonStateHover;
    property StateClicked: TBCButtonState read FStateClicked write SeTBCButtonStateClicked;
    property Down: boolean read FDown write SetDown default False;
    property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth;
    property DropDownArrowSize: Integer read FDropDownArrowSize write SetDropDownArrowSize;
    property FlipArrow: boolean read FFlipArrow write SetFlipArrow;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphMargin: integer read FGlyphMargin write SetGlyphMargin default 5;
    property Style: TBCButtonStyle read FStyle write SetStyle default bbtButton;
    property StaticButton: boolean
      read FStaticButton write SetStaticButton default False;
    property GlobalOpacity: byte read FGlobalOpacity write SetGlobalOpacity;
    property Rounding: TBCRounding read FRounding write SetRounding;
    property RoundingDropDown: TBCRounding read FRoundingDropDown write SetRoundingDropDown;
    property TextApplyGlobalOpacity: boolean
      read FTextApplyGlobalOpacity write SetTextApplyGlobalOpacity;
    property OnAfterRenderBCButton: TOnAfterRenderBCButton
      read FOnAfterRenderBCButton write FOnAfterRenderBCButton;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetSizeVariables(newDropDownWidth, newDropDownArrowSize,
      newAutoSizeExtraVertical, newAutoSizeExtraHorizontal: integer);
    procedure UpdateControl; override; // Called by EndUpdate
  end;

  TBCButton = class(TCustomBCButton)
  published
    property Action;
    property Align;
    property Anchors;
    property AssignStyle;
    property AutoSize;
    property StateClicked;
    property StateHover;
    property StateNormal;
    property BorderSpacing;
    property Caption;
    property Color;
    property Down;
    property DropDownWidth;
    property DropDownArrowSize;
    property Enabled;
    property FlipArrow;
    property GlobalOpacity;
    property Glyph;
    property GlyphMargin;
    property OnAfterRenderBCButton;
    property OnButtonClick;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property ParentColor;
    property PopupMenu;
    property Rounding;
    property RoundingDropDown;
    property StaticButton;
    property Style;
    property TextApplyGlobalOpacity;
    property Visible;
  end;

procedure Register;

implementation

uses LCLIntf, Math, LCLProc, BGRAPolygon, BCTools, SysUtils;

procedure Register;
begin
  {$I bcbutton_icon.lrs}
  RegisterComponents('BGRA Controls', [TBCButton]);
end;

{ TBCButtonState }

procedure TBCButtonState.SetFontEx(const AValue: TBCFont);
begin
  if FFontEx = AValue then
    exit;
  FFontEx.Assign(AValue);

  Change;
end;

procedure TBCButtonState.OnChangeFont(Sender: TObject; AData: PtrInt);
begin
  Change(PtrInt(pdUpdateSize));
end;

procedure TBCButtonState.OnChangeChildProperty(Sender: TObject; AData: PtrInt);
begin
  Change(AData);
end;

procedure TBCButtonState.SetBackground(AValue: TBCBackground);
begin
  if FBackground = AValue then Exit;
  FBackground.Assign(AValue);

  Change;
end;

procedure TBCButtonState.SetBorder(AValue: TBCBorder);
begin
  if FBorder = AValue then Exit;
  FBorder.Assign(AValue);

  Change;
end;

constructor TBCButtonState.Create(AControl: TControl);
begin
  FBackground := TBCBackground.Create(AControl);
  FBorder     := TBCBorder.Create(AControl);
  FFontEx     := TBCFont.Create(AControl);

  FBackground.OnChange := @OnChangeChildProperty;
  FBorder.OnChange     := @OnChangeChildProperty;
  FFontEx.OnChange     := @OnChangeFont;

  inherited Create(AControl);
end;

destructor TBCButtonState.Destroy;
begin
  FBackground.Free;
  FBorder.Free;
  FFontEx.Free;
  inherited Destroy;
end;

procedure TBCButtonState.Assign(Source: TPersistent);
begin
  if Source is TBCButtonState then
  begin
    FBackground.Assign(TBCButtonState(Source).FBackground);
    FBorder.Assign(TBCButtonState(Source).FBorder);
    FFontEx.Assign(TBCButtonState(Source).FFontEx);

    Change(PtrInt(pdUpdateSize));
  end
  else
    inherited Assign(Source);
end;

{ TCustomBCButton }

procedure TCustomBCButton.AssignDefaultStyle;
begin
  FRounding.RoundX := 12;
  FRounding.RoundY := 12;
  // Normal
  with StateNormal do
  begin
    Border.Style         := bboNone;
    FontEx.Color         := RGBToColor(230,230,255);
    FontEx.Style         := [fsBold];
    FontEx.Shadow        := True;
    FontEx.ShadowOffsetX := 1;
    FontEx.ShadowOffsetY := 1;
    FontEx.ShadowRadius  := 2;
    Background.Gradient1EndPercent := 60;
    Background.Style               := bbsGradient;
    // Gradient1
    with Background.Gradient1 do
    begin
      EndColor   := RGBToColor(64,64,128);
      StartColor := RGBToColor(0,0,64);
    end;
    // Gradient2
    with Background.Gradient2 do
    begin
      EndColor       := RGBToColor(0,0,64);
      GradientType   := gtRadial;
      Point1XPercent := 50;
      Point1YPercent := 100;
      Point2YPercent := 0;
      StartColor     := RGBToColor(64,64,128);
    end;
  end;
  // Hover
  with StateHover do
  begin
    Border.Style         := bboNone;
    FontEx.Color         := RGBToColor(255,255,255);
    FontEx.Style         := [fsBold];
    FontEx.Shadow        := True;
    FontEx.ShadowOffsetX := 1;
    FontEx.ShadowOffsetY := 1;
    FontEx.ShadowRadius  := 2;
    Background.Gradient1EndPercent := 100;
    Background.Style               := bbsGradient;
    // Gradient1
    with Background.Gradient1 do
    begin
      EndColor       := RGBToColor(0,64,128);
      GradientType   := gtRadial;
      Point1XPercent := 50;
      Point1YPercent := 100;
      Point2YPercent := 0;
      StartColor     := RGBToColor(0,128,255);
    end;
  end;
  // Clicked
  with StateClicked do
  begin
    Border.Style         := bboNone;
    FontEx.Color         := RGBToColor(230,230,255);
    FontEx.Style         := [fsBold];
    FontEx.Shadow        := True;
    FontEx.ShadowOffsetX := 1;
    FontEx.ShadowOffsetY := 1;
    FontEx.ShadowRadius  := 2;
    Background.Gradient1EndPercent := 100;
    Background.Style               := bbsGradient;
    // Gradient1
    with Background.Gradient1 do
    begin
      EndColor       := RGBToColor(0,0,64);
      GradientType   := gtRadial;
      Point1XPercent := 50;
      Point1YPercent := 100;
      Point2YPercent := 0;
      StartColor     := RGBToColor(0,64,128);
    end;
  end;
end;

procedure TCustomBCButton.CalculateGlyphSize(var NeededWidth, NeededHeight: integer);
begin
  if FGlyph = nil then
  begin
    NeededHeight := 0;
    NeededWidth := 0;
    Exit;
  end;

  NeededWidth := FGlyph.Width;
  NeededHeight := FGlyph.Height;
end;

procedure TCustomBCButton.ConvertToGrayScale(ABGRA: TBGRABitmap);
var
  bounds: TRect;
  px: PBGRAPixel;
  xb, yb: integer;
begin
  bounds := ABGRA.GetImageBounds;
  if (bounds.Right <= bounds.Left) or (bounds.Bottom <= Bounds.Top) then
    exit;

  for yb := bounds.Top to bounds.bottom - 1 do
  begin
    px := ABGRA.scanline[yb] + bounds.left;
    for xb := bounds.left to bounds.right - 1 do
    begin
      px^ := BGRAToGrayscale(px^);
      Inc(px);
    end;
  end;
  ABGRA.InvalidateBitmap;
end;

procedure TCustomBCButton.RenderAll(ANow: Boolean);
begin
  if (csCreating in FControlState) or IsUpdating or (FBGRANormal=nil) then
    Exit;

  if ANow then
  begin
    Render(FBGRANormal, FStateNormal);
    Render(FBGRAHover, FStateHover);
    Render(FBGRAClick, FStateClicked);
  end else
  begin
    FBGRANormal.NeedRender := True;
    FBGRAHover.NeedRender := True;
    FBGRAClick.NeedRender := True;
  end;
end;

function TCustomBCButton.GetButtonRect: TRect;
begin
  Result := GetClientRect;
  if FStyle=bbtDropDown then
    Dec(Result.Right,GetDropDownWidth(False));
end;

function TCustomBCButton.GetDropDownWidth(AFull: boolean): integer;
begin
  Result := FDropDownWidth + (ifthen(AFull, 2, 1) * FStateNormal.FBorder.Width);
end;

function TCustomBCButton.GetDropDownRect(AFull: Boolean): TRect;
begin
  Result := GetClientRect;
  Result.Left := Result.Right - GetDropDownWidth(AFull);
end;

procedure TCustomBCButton.Render(ABGRA: TBGRABitmapEx;
  AState: TBCButtonState);
var
  r,r_a: TRect;

  { TODO: Create customizable glyph position by creating TBCGlyph type
          and method in BCTools which render it }
  procedure _RenderGlyph;
  var
    w, h, t, l: Integer;
    g: TBGRABitmap;
  begin
    if (FGlyph<>nil) and (not FGlyph.Empty) then
    begin
      CalculateTextSize(Caption,AState.FontEx,w,h);
      l := r.Right - Round(((r.Right-r.Left) + w + FGlyph.Width)/2);
      t := r.Bottom - Round(((r.Bottom-r.Top) + FGlyph.Height) / 2);
      g := TBGRABitmap.Create(Glyph);
      ABGRA.PutImage(l,t,g,dmDrawWithTransparency);
      g.Free;
      Inc(r.Left,l+FGlyph.Width+FGlyphMargin);
    end;
  end;

begin
  if (csCreating in FControlState) or IsUpdating then
    Exit;

  ABGRA.NeedRender := False;

  { Refreshing size }
  ABGRA.SetSize(Width, Height);

  { Calculating rect }
  r   := GetButtonRect;
  CalculateBorderRect(AState.Border,r);

  if FStyle = bbtDropDown then
  begin
    r_a := GetDropDownRect;
    CalculateBorderRect(AState.Border,r_a);
  end;

  { Clearing previous paint }
  ABGRA.Fill(BGRAPixelTransparent);
  { Basic body }
  RenderState(ABGRA, AState, r, FRounding);
  if FStyle = bbtDropDown then
  begin
    RenderState(ABGRA, AState, r_a, FRoundingDropDown);
    // Click offset for arrow
    if AState=FStateClicked then
    begin
      Inc(r_a.Left,2);
      Inc(r_a.Top,2);
    end;

    if FFlipArrow then
      RenderArrow(TBGRABitmap(ABGRA),r_a,FDropDownArrowSize,badUp,AState.FontEx.Color)
    else
      RenderArrow(TBGRABitmap(ABGRA),r_a,FDropDownArrowSize,badDown,AState.FontEx.Color);
  end;

  // Click offset for text and glyph
  if AState=FStateClicked then
  begin
    Inc(r.Left,2);
    Inc(r.Top,2);
  end;

  if FTextApplyGlobalOpacity then
  begin
    { Drawing text }
    _RenderGlyph;
    RenderText(r,AState.FontEx,Self.Caption,TBGRABitmap(ABGRA));

    { Set global opacity }
    ABGRA.ApplyGlobalOpacity(FGlobalOpacity);
  end
  else
  begin
    { Set global opacity }
    ABGRA.ApplyGlobalOpacity(FGlobalOpacity);
    { Drawing text }
    _RenderGlyph;
    RenderText(r,AState.FontEx,Self.Caption,TBGRABitmap(ABGRA));
  end;

  { Convert to gray if not enabled }
  if not Enabled then
    ConvertToGrayScale(ABGRA);

  if Assigned(FOnAfterRenderBCButton) then
    FOnAfterRenderBCButton(Self, ABGRA, AState, r);

  {$IFDEF DEBUG}
  FRenderCount += 1;
  {$ENDIF}
end;

procedure TCustomBCButton.RenderState(ABGRA: TBGRABitmapEx;
  AState: TBCButtonState; const ARect: TRect; ARounding: TBCRounding);
begin
  RenderBackground(ARect, AState.FBackground, TBGRABitmap(ABGRA), ARounding);
  RenderBorder(ARect, AState.FBorder, TBGRABitmap(ABGRA), ARounding);
end;

procedure TCustomBCButton.OnChangeGlyph(Sender: TObject);
begin
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.OnChangeState(Sender: TObject; AData: PtrInt);
begin
  RenderControl;
  if TBCButtonPropertyData(AData)=pdUpdateSize then
    UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SeTBCButtonStateClicked(const AValue: TBCButtonState);
begin
  if FStateClicked = AValue then
    exit;
  FStateClicked.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SeTBCButtonStateHover(const AValue: TBCButtonState);
begin
  if FStateHover = AValue then
    exit;
  FStateHover.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SeTBCButtonStateNormal(const AValue: TBCButtonState);
begin
  if FStateNormal = AValue then
    exit;
  FStateNormal.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetDown(AValue: boolean);
begin
  if FDown = AValue then
    exit;
  FDown := AValue;
  if FDown
  then FButtonState := msClicked
  else FButtonState := msNone;
  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetDropDownArrowSize(AValue: Integer);
begin
  if FDropDownArrowSize = AValue then Exit;
  FDropDownArrowSize := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetDropDownWidth(AValue: Integer);
begin
  if FDropDownWidth = AValue then Exit;
  FDropDownWidth := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetFlipArrow(AValue: boolean);
begin
  if FFlipArrow = AValue then
    Exit;
  FFlipArrow := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetGlyph(const AValue: TBitmap);
begin
  if (FGlyph <> nil) and (FGlyph = AValue) then
    exit;

  FGlyph.Assign(AValue);

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetGlyphMargin(const AValue: integer);
begin
  if FGlyphMargin = AValue then
    exit;
  FGlyphMargin := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.SetRounding(AValue: TBCRounding);
begin
  if FRounding = AValue then Exit;
  FRounding.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetRoundingDropDown(AValue: TBCRounding);
begin
  if FRoundingDropDown = AValue then Exit;
  FRoundingDropDown.Assign(AValue);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetStaticButton(const AValue: boolean);
begin
  if FStaticButton = AValue then
    exit;
  FStaticButton := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetStyle(const AValue: TBCButtonStyle);
begin
  if FStyle = AValue then
    exit;
  FStyle := AValue;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.UpdateSize;
begin
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomBCButton.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
var
  AWidth: integer;
  gh: integer = 0;
  gw: integer = 0;
begin
  if (Parent = nil) or (not Parent.HandleAllocated) then
    Exit;
  if WidthIsAnchored then
    AWidth := Width
  else
    AWidth := 10000;

  CalculateTextSize(Caption, FStateNormal.FontEx, PreferredWidth, PreferredHeight);

  // Extra pixels for DropDown
  if Style = bbtDropDown then
    Inc(PreferredWidth, GetDropDownWidth);

  CalculateGlyphSize(gw, gh);

  if (FGlyph <> nil) and (not FGlyph.Empty) then
  begin
    if Caption = '' then
    begin
      Inc(PreferredWidth, gw{ - AutoSizeExtraY * 2});
      Inc(PreferredHeight, gh);
    end
    else
    begin
      Inc(PreferredWidth, gw + FGlyphMargin);
      if gh > PreferredHeight then
        PreferredHeight := gh;
    end;
  end;

  // Extra pixels for AutoSize
  Inc(PreferredWidth, AutoSizeExtraX);
  Inc(PreferredHeight, AutoSizeExtraY);
end;

class function TCustomBCButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 123;
  Result.CY := 33;
end;

procedure TCustomBCButton.Click;
begin
  if (FActiveButt = bbtDropDown) and Assigned(FOnButtonClick) then
  begin
    FOnButtonClick(Self);
    Exit;
  end;
  inherited Click;
end;

procedure TCustomBCButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then
    exit;

  if (Button = mbLeft) and Enabled {and (not (FButtonState = msClicked)) }then
  begin
    case FActiveButt of
      bbtButton:
        if not (FButtonState=msClicked) then
        begin
          FButtonState     := msClicked;
          FDownButtonState := msNone;
          Invalidate;
        end;
      bbtDropDown:
        if not (FDownButtonState=msClicked) then
        begin
          FButtonState     := msNone;
          FDownButtonState := msClicked;
          Invalidate;
        end;
    end;
    // Old
    {FButtonState := msClicked;
    Invalidate;}
  end;
end;

procedure TCustomBCButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  p: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if csDesigning in ComponentState then
    exit;

  if (Button = mbLeft) and Enabled {and (FButtonState = msClicked)} then
  begin
    case FActiveButt of
      bbtButton:
        if FButtonState=msClicked then
        begin
          FButtonState     := msHover;
          FDownButtonState := msNone;
          Invalidate;
        end;
      bbtDropDown:
        if FDownButtonState = msClicked then
        begin
          FDownButtonState := msHover;
          FButtonState     := msNone;
          Invalidate;
        end;
    end;
    // Old
    {FButtonState := msHover;
    Invalidate;}
  end;

  if (FActiveButt = bbtDropDown) and (PopupMenu <> nil) and Enabled then
  begin
    if FFlipArrow then
      p := ClientToScreen(Point(Width - FDropDownWidth - (FStateNormal.FBorder.Width * 2),
        {PopupMenu.Height} -1))
    else
      p := ClientToScreen(Point(Width - FDropDownWidth - (FStateNormal.FBorder.Width * 2), Height + 1));

    PopupMenu.PopUp(p.x, p.y);
    //p := ClientToScreen(Point(X, Y));
    //PopupMenu.PopUp(p.x, p.y);
  end;
end;

procedure TCustomBCButton.MouseEnter;
begin
  if csDesigning in ComponentState then
    exit;
  case FActiveButt of
    bbtButton:
      begin
        if FDown
        then FButtonState := msClicked
        else FButtonState := msHover;
        FDownButtonState  := msNone;
      end;
    bbtDropDown:
      begin
        if FDown
        then FButtonState := msClicked
        else FButtonState := msNone;
        FDownButtonState  := msHover;
      end;
  end;
  Invalidate;
  // Old
  {FButtonState := msHover;
  Invalidate;}
  inherited MouseEnter;
end;

procedure TCustomBCButton.MouseLeave;
begin
  if csDesigning in ComponentState then
    exit;
  if FDown then
  begin
    FButtonState := msClicked;
    FActiveButt := bbtButton;
  end
  else
    FButtonState := msNone;
  FDownButtonState  := msNone;
  Invalidate;
  inherited MouseLeave;
end;

procedure TCustomBCButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);

  if FStyle=bbtButton then
    FActiveButt := bbtButton
  else
  begin
    // Calling invalidate only when active button changed. Otherwise, we leave
    // this for LCL. This reduce paint call
    if (FActiveButt=bbtButton) and (x>GetButtonRect.Right) then
    begin
      FActiveButt      := bbtDropDown;
      FDownButtonState := msHover;
      if FDown
      then FButtonState := msClicked
      else FButtonState := msNone;
      Invalidate;
    end else
    if (FActiveButt=bbtDropDown) and (x<=GetButtonRect.Right) then
    begin
      FActiveButt      := bbtButton;
      if FDown
      then FButtonState := msClicked
      else FButtonState := msHover;
      FDownButtonState := msNone;
      Invalidate;
    end;
  end;
end;

procedure TCustomBCButton.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.TextChanged;
begin
  inherited TextChanged;
  RenderControl;
  UpdateSize;
  Invalidate;
end;

procedure TCustomBCButton.UpdateControl;
begin
  RenderControl;
  inherited UpdateControl; // indalidate
end;

{$IFDEF DEBUG}
function TCustomBCButton.GetDebugText: String;
begin
  Result := 'R: '+IntToStr(FRenderCount);
end;
{$ENDIF}

procedure TCustomBCButton.DrawControl;
var
  bgra: TBGRABitmapEx;
begin

  // If style is without dropdown button or state of each button
  // is the same (possible only for msNone) or static button then
  // we can draw whole BGRABitmap
  if (FStyle=bbtButton) or (FButtonState=FDownButtonState) or FStaticButton
  then
  begin
    // Main button
    if FStaticButton then
      bgra := FBGRANormal
    else
    if FDown then
      bgra := FBGRAClick
    else
      case FButtonState of
        msNone: bgra := FBGRANormal;
        msHover: bgra := FBGRAHover;
        msClicked: bgra := FBGRAClick;
      end;
    if bgra.NeedRender then
      Render(bgra,TBCButtonState(bgra.CustomData));
    bgra.Draw(Self.Canvas, 0, 0, False);
  end
  // Otherwise we must draw part of state for each button
  else
  begin
    // The active button must be draw as last because right edge of button and
    // left edge of dropdown are overlapping each other, so we must draw edge
    // for current state of active button
    case FActiveButt of
      bbtButton:
        begin
          // Drop down button
          case FDownButtonState of
            msNone: bgra := FBGRANormal;
            msHover: bgra := FBGRAHover;
            msClicked: bgra := FBGRAClick;
          end;
          if bgra.NeedRender then
            Render(bgra,TBCButtonState(bgra.CustomData));
          bgra.DrawPart(GetDropDownRect,Self.Canvas,GetDropDownRect.Left,0,False);
          // Main button
          if FDown then
            bgra := FBGRAClick
          else
            case FButtonState of
              msNone: bgra := FBGRANormal;
              msHover: bgra := FBGRAHover;
              msClicked: bgra := FBGRAClick;
            end;
          if bgra.NeedRender then
            Render(bgra,TBCButtonState(bgra.CustomData));
          bgra.DrawPart(GetButtonRect, Self.Canvas, 0, 0, False);
        end;
      bbtDropDown:
        begin
          // Main button
          if FDown then
            bgra := FBGRAClick
          else
            case FButtonState of
              msNone: bgra := FBGRANormal;
              msHover: bgra := FBGRAHover;
              msClicked: bgra := FBGRAClick;
            end;
          if bgra.NeedRender then
            Render(bgra,TBCButtonState(bgra.CustomData));
          bgra.DrawPart(GetButtonRect, Self.Canvas, 0, 0, False);
          // Drop down button
          case FDownButtonState of
            msNone: bgra := FBGRANormal;
            msHover: bgra := FBGRAHover;
            msClicked: bgra := FBGRAClick;
          end;
          if bgra.NeedRender then
            Render(bgra,TBCButtonState(bgra.CustomData));
          bgra.DrawPart(GetDropDownRect,Self.Canvas,GetDropDownRect.Left,0,False);
        end;
    end;
  end;
end;

procedure TCustomBCButton.RenderControl;
begin
  inherited RenderControl;
  RenderAll;
end;

procedure TCustomBCButton.SetGlobalOpacity(const AValue: byte);
begin
  if FGlobalOpacity = AValue then
    exit;
  FGlobalOpacity := AValue;

  RenderControl;
  Invalidate;
end;

procedure TCustomBCButton.SetTextApplyGlobalOpacity(const AValue: boolean);
begin
  if FTextApplyGlobalOpacity = AValue then
    exit;
  FTextApplyGlobalOpacity := AValue;

  RenderControl;
  Invalidate;
end;

constructor TCustomBCButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DEBUG}
  FRenderCount := 0;
  {$ENDIF}
  DisableAutoSizing;
  Include(FControlState, csCreating);
  //{$IFDEF WINDOWS}
  // default sizes under different dpi settings
  //SetSizeVariables(ScaleX(8,96), ScaleX(16,96), ScaleY(8,96), ScaleX(24,96));
  //{$ELSE}
  // default sizes
  SetSizeVariables(16, 8, 8, 24);
  //{$ENDIF}
  BeginUpdate;
  try
    with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
    ControlStyle := ControlStyle + [csAcceptsControls];
    FBGRANormal := TBGRABitmapEx.Create(Width, Height, BGRAPixelTransparent);
    FBGRAHover := TBGRABitmapEx.Create(Width, Height, BGRAPixelTransparent);
    FBGRAClick := TBGRABitmapEx.Create(Width, Height, BGRAPixelTransparent);

    ParentColor := False;
    Color := clNone;

    FStateNormal           := TBCButtonState.Create(Self);
    FStateHover            := TBCButtonState.Create(Self);
    FStateClicked          := TBCButtonState.Create(Self);
    FStateNormal.OnChange  := @OnChangeState;
    FStateHover.OnChange   := @OnChangeState;
    FStateClicked.OnChange := @OnChangeState;

    FRounding              := TBCRounding.Create(Self);
    FRounding.OnChange     := @OnChangeState;

    FRoundingDropDown      := TBCRounding.Create(Self);
    FRoundingDropDown.OnChange := @OnChangeState;

    { Connecting bitmaps with states property to easy call and access }
    FBGRANormal.CustomData := PtrInt(FStateNormal);
    FBGRAHover.CustomData  := PtrInt(FStateHover);
    FBGRAClick.CustomData  := PtrInt(FStateClicked);

    FButtonState := msNone;
    FDownButtonState := msNone;
    FFlipArrow := False;
    FGlyph := TBitmap.Create;
    FGlyph.OnChange := @OnChangeGlyph;
    FGlyphMargin := 5;
    FStyle := bbtButton;
    FStaticButton := False;
    FActiveButt := bbtButton;
    FGlobalOpacity := 255;
    FTextApplyGlobalOpacity := False;
    //FStates := [];
    FDown := False;

    { Default style }
    AssignDefaultStyle;
  finally
    Exclude(FControlState, csCreating);
    EnableAutoSizing;
    EndUpdate;
  end;
end;

destructor TCustomBCButton.Destroy;
begin
  FStateNormal.Free;
  FStateHover.Free;
  FStateClicked.Free;
  FBGRANormal.Free;
  FBGRAHover.Free;
  FBGRAClick.Free;
  FreeThenNil(FGlyph);
  FRounding.Free;
  FRoundingDropDown.Free;
  inherited Destroy;
end;

procedure TCustomBCButton.Assign(Source: TPersistent);
begin
  if Source is TCustomBCButton then
  begin
    Glyph := TCustomBCButton(Source).Glyph;
    FGlyphMargin := TCustomBCButton(Source).FGlyphMargin;
    FStyle := TCustomBCButton(Source).FStyle;
    FFlipArrow := TCustomBCButton(Source).FFlipArrow;
    FStaticButton := TCustomBCButton(Source).FStaticButton;
    FGlobalOpacity := TCustomBCButton(Source).FGlobalOpacity;
    FTextApplyGlobalOpacity := TCustomBCButton(Source).FTextApplyGlobalOpacity;
    FStateNormal.Assign(TCustomBCButton(Source).FStateNormal);
    FStateHover.Assign(TCustomBCButton(Source).FStateHover);
    FStateClicked.Assign(TCustomBCButton(Source).FStateClicked);
    FDropDownArrowSize := TCustomBCButton(Source).FDropDownArrowSize;
    FDropDownWidth := TCustomBCButton(Source).FDropDownWidth;
    AutoSizeExtraX := TCustomBCButton(Source).AutoSizeExtraX;
    AutoSizeExtraY := TCustomBCButton(Source).AutoSizeExtraY;
    FDown := TCustomBCButton(Source).FDown;
    FRounding.Assign(TCustomBCButton(Source).FRounding);
    FRoundingDropDown.Assign(TCustomBCButton(Source).FRoundingDropDown);

    RenderControl;
    Invalidate;
    UpdateSize;
  end
  else
    inherited Assign(Source);
end;

procedure TCustomBCButton.SetSizeVariables(newDropDownWidth, newDropDownArrowSize,
  newAutoSizeExtraVertical, newAutoSizeExtraHorizontal: integer);
begin
  FDropDownArrowSize := newDropDownArrowSize;
  FDropDownWidth     := newDropDownWidth;
  AutoSizeExtraY     := newAutoSizeExtraVertical;
  AutoSizeExtraX     := newAutoSizeExtraHorizontal;

  if csCreating in ControlState then
    Exit;

  RenderControl;
  UpdateSize;
  Invalidate;
end;

function TCustomBCButton.GetStyleExtension: String;
begin
  Result := 'bcbtn';
end;

end.
