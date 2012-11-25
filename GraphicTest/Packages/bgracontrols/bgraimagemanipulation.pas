unit BGRAImageManipulation;

{ ============================================================================
  BGRAImageManipulation Unit

  Copyright (C) 2011 - Emerson Cavalcanti <emersoncavalcanti at googlesites>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ============================================================================
  Description:

  TBGRAImageManipulation is a component designed to make simple changes in an
  image while maintaining the aspect ratio of the final image and allow it to
  cut to reduce the unnecessary edges. The selected area is painted with a
  different transparency level for easy viewing of what will be cut.

  ============================================================================
  History:

  2011-05-03 - Emerson Cavalcanti
             - Initial version

  2011-06-01 - Emerson Cavalcanti
             - Fixed aspect ratio when the image has a dimension smaller than
               the size of the component.
             - Fixed memory leak on temporary bitmaps.
             - Fixed unecessary release of bitmap.
             - Inserted Anchor and Align property on component.
             - Implemented 'Keep aspect Ratio' property. Now you can select an
               area without maintaining the aspect ratio.

  2011-06-03 - Emerson Cavalcanti
             - Improved selection when don't use aspect ratio.
             - Improved response when resize component.
             - Fixed memory leak on resample bitmap.

  2011-06-04 - Circular
             - Fixed divide by zero when calculate aspect ratio on
               getImageRect.

  2011-06-07 - Emerson Cavalcanti
             - Improved function of aspect ratio including a variable to
               provide the value directly in the component, instead of using
               the dimensions of the component as the source of this value.
             - Improved exhibition of anchors on selection.
             - Improved mouse cursor.
             - Included function to get the aspect ratio from image size.
             - Included rotate Left and Right functions.

  ============================================================================
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, BGRABitmap, BGRABitmapTypes,
  Graphics, Dialogs, LCLIntf, BGRAGradientScanner;

{$INCLUDE DataType.inc}

type
  TBGRAImageManipulation = class(TGraphicControl)
  private
    { Private declarations }

    fAnchorSize:      byte;
    fAnchorSelected:  TDirection;
    fBorderSize:      byte;
    fAspectRatio:     string;
    fAspectX:         integer;
    fAspectY:         integer;
    fKeepAspectRatio: boolean;
    fMinHeight:       integer;
    fMinWidth:        integer;
    fMouseCaught:     boolean;
    fStartPoint:      TPoint;
    fEndPoint:        TPoint;

    fGCD:        integer;
    fRatio:      TRatio;
    fSizeLimits: TSizeLimits;

    fImageBitmap, fResampledBitmap, fBackground, fVirtualScreen: TBGRABitmap;

    fCropArea:        TRect;
    fDeltaX, fDeltaY: integer;

    function getAnchorSize: byte;
    procedure setAnchorSize(const Value: byte);
    function getEmpty: boolean;
    procedure setBitmap(const Value: TBGRABitmap);
    procedure setBorderSize(const Value: byte);
    procedure setAspectRatio(const Value: string);
    procedure setKeepAspectRatio(const Value: boolean);
    procedure setMinHeight(const Value: integer);
    procedure setMinWidth(const Value: integer);
  protected
    { Protected declarations }

    function ApplyDimRestriction(Coords: TCoord; Direction: TDirection;
      Bounds: TRect): TCoord;
    function ApplyRatioToAxes(Coords: TCoord; Direction: TDirection;
      Bounds: Trect): TCoord;
    procedure CalcMaxSelection;
    procedure findSizeLimits;
    function getDirection(const Point1, Point2: TPoint): TDirection;
    function getGCD(Nr1, Nr2: longint): longint;
    function getLCM(Nr1, Nr2: longint): longint;
    function getImageRect(Picture: TBGRABitmap): TRect;
    function getWorkRect: TRect;
    function isOverAnchor(Point: TPoint; Corner: TPoint): boolean;

    procedure Paint; override;
    procedure RepaintBackground;
    procedure Resize; override;
    procedure Render;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    function getAspectRatioFromImage(const Value: TBGRABitmap): string;
    function getBitmap: TBGRABitmap;

    procedure rotateLeft;
    procedure rotateRight;
  published
    { Published declarations }

    property Align;
    property Anchors;

    property AnchorSize: byte Read getAnchorSize Write setAnchorSize default 5;
    property Bitmap: TBGRABitmap Read fImageBitmap Write setBitmap;
    property BorderSize: byte Read fBorderSize Write setBorderSize default 2;
    property AspectRatio: string Read fAspectRatio Write setAspectRatio;
    property KeepAspectRatio: boolean Read fKeepAspectRatio
      Write setKeepAspectRatio default True;
    property MinHeight: integer Read fMinHeight Write setMinHeight;
    property MinWidth: integer Read fMinWidth Write setMinWidth;
    property Empty: boolean Read getEmpty;
  end;

procedure Register;

implementation

uses Math, ExtCtrls;

resourcestring
  SAnchorSizeIsTooLarge =
    'Anchor size is too large. %d is not within the valid range of %d..%d';
  SAnchorSizeIsTooSmall =
    'Anchor size is too small. %d is not within the valid range of %d..%d';
  SAnchorSizeIsNotOdd = 'Anchor size is invalid. %d is not an odd number.';

  SBorderSizeIsTooLarge =
    'Border size is too large. %d is not within the valid range of %d..%d';
  SBorderSizeIsTooSmall =
    'Border size is too small. %d is not within the valid range of %d..%d';

  SAspectRatioIsNotValid = 'Aspect ratio value is invalid. %s contain invalid number.';

 { ============================================================================ }
 { =====[ Auxiliary Functions ]================================================ }
 { ============================================================================ }

{ Applies the given size constraint on the coordinates along both axes }
function TBGRAImageManipulation.ApplyDimRestriction(Coords: TCoord;
  Direction: TDirection; Bounds: TRect): TCoord;
var
  newCoords: TCoord;
  calcWidth, calcHeight: integer;
  recalculateHeight: boolean;
begin
  // Gets coordinates
  newCoords := Coords;
  recalculateHeight := False;

  // Calculated width
  calcWidth  := abs(newCoords.x2 - newCoords.x1);
  calcHeight := abs(newCoords.y2 - newCoords.y1);

  // Checks if the width is smaller than the minimum value
  if (Abs(calcWidth) < MinWidth) and (MinWidth < fImageBitmap.Width) then
  begin
    // Resizes the width based on the minimum value
    calcWidth := MinWidth;

    if (EAST in Direction) then
    begin
      // If the motion is in a positive direction, make sure we're not going out
      // of bounds
      if ((newCoords.x1 + calcWidth) > Bounds.Right) then
      begin
        // Moves the horizontal coordinates
        newCoords.x1 := Bounds.Right - calcWidth;
        newCoords.x2 := Bounds.Right;
      end
      else
      begin
        // Moves the last horizontal coordinate
        newCoords.x2 := newCoords.x1 + calcWidth;
      end;
    end
    else
    begin
      // If the motion is in a negative direction, make sure we're not going out
      // of bounds
      if ((newCoords.x1 - calcWidth) < Bounds.Left) then
      begin
        // Moves the horizontal coordinates
        newCoords.x1 := Bounds.Left + calcWidth;
        newCoords.x2 := Bounds.Left;
      end
      else
      begin
        // Moves the last horizontal coordinate
        newCoords.x2 := newCoords.x1 - calcWidth;
      end;
    end;

    if (fKeepAspectRatio) then
    begin
      // Resizes the height based on the minimum value
      recalculateHeight := True;
    end;
  end;

  // Checks if the height is smaller than the minimum value
  if (((Abs(calcHeight) < MinHeight) and (MinHeight < fImageBitmap.Height)) or
    recalculateHeight) then
  begin
    // Resizes the height based on the minimum value
    calcHeight := MinHeight;

    if (SOUTH in Direction) then
    begin
      // If the motion is in a positive direction, make sure we're not going out
      // of bounds
      if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
      begin
        // Moves the vertical coordinates
        newCoords.y1 := Bounds.Bottom - calcHeight;
        newCoords.y2 := Bounds.Bottom;
      end
      else
      begin
        // Moves the last horizontal coordinate
        newCoords.y2 := newCoords.y1 + calcHeight;
      end;
    end
    else
    begin
      // If the motion is in a negative direction, make sure we're not going out
      // of bounds
      if ((newCoords.y1 - calcHeight) < Bounds.Top) then
      begin
        // Moves the vertical coordinates
        newCoords.y1 := Bounds.Top + calcHeight;
        newCoords.y2 := Bounds.Top;
      end
      else
      begin
        // Moves the last horizontal coordinate
        newCoords.y2 := newCoords.y1 - calcHeight;
      end;
    end;
  end;

  Result := newCoords;
end;

 { Applies the provided ratio to the coordinates based on direction and bounds  }
 { on both axes.                                                                }
function TBGRAImageManipulation.ApplyRatioToAxes(Coords: TCoord;
  Direction: TDirection; Bounds: TRect): TCoord;
var
  newCoords: TCoord;
  calcWidth, calcHeight: integer;
  RecalculatesOtherAxis: boolean;
begin
  // Gets coordinates
  newCoords := Coords;

  // Check if movement is only vertical
  if ((fAnchorSelected = [NORTH]) or (fAnchorSelected = [SOUTH])) then
  begin
    // Vertical movement: keep current width
    if (fKeepAspectRatio) then
    begin
      // Calculate height
      calcHeight := newCoords.y2 - newCoords.y1;

      // Make sure we're not going out of bounds
      if (SOUTH in Direction) then
      begin
        if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
        begin
          calcHeight := Bounds.Bottom - newCoords.y1; // Limite height dimension
          newCoords.y2 := Bounds.Bottom;
        end;
      end
      else
      begin
        if ((newCoords.y1 + calcHeight) < Bounds.Top) then
        begin
          calcHeight := -(newCoords.y1 - Bounds.Top); // Limite height dimension
          newCoords.y2 := Bounds.Top;
        end;
      end;

      // Calculate the new width based on the proportion of height
      calcWidth := Trunc(abs(calcHeight) * (fRatio.Horizontal / fRatio.Vertical));

      // Make sure we're not going out of bounds
      if (fAnchorSelected = [NORTH]) then
      begin
        if ((newCoords.x1 - calcWidth) < Bounds.Left) then
        begin
          calcWidth := newCoords.x1 - Bounds.Left; // Limite width dimension
          newCoords.x2 := Bounds.Left;
          RecalculatesOtherAxis := True;
        end;
      end
      else
      begin
        if ((newCoords.x1 + calcWidth) > Bounds.Right) then
        begin
          calcWidth := Bounds.Right - newCoords.x1; // Limite width dimension
          newCoords.x2 := Bounds.Right;
          RecalculatesOtherAxis := True;
        end;
      end;

      // Apply calculated dimensions of width on height
      if (RecalculatesOtherAxis) then
      begin
        if (calcHeight > 0) then
          calcHeight := Trunc(calcWidth * (fRatio.Vertical / fRatio.Horizontal))
        else
          calcHeight := -Trunc(calcWidth * (fRatio.Vertical / fRatio.Horizontal));

        newCoords.y2 := newCoords.y1 + calcHeight;
      end;
    end
    else
    begin
      // Calculate height
      calcHeight := newCoords.y2 - newCoords.y1;

      // Make sure we're not going out of bounds
      if (SOUTH in Direction) then
      begin
        if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
        begin
          calcHeight := Bounds.Bottom - newCoords.y1; // Limite height dimension
          newCoords.y2 := Bounds.Bottom;
        end;
      end
      else
      begin
        if ((newCoords.y1 + calcHeight) < Bounds.Top) then
        begin
          calcHeight := -(newCoords.y1 - Bounds.Top); // Limite height dimension
          newCoords.y2 := Bounds.Top;
        end;
      end;

      // Calculate width
      calcWidth := abs(fCropArea.Right - fCropArea.Left);
    end;

    if (fAnchorSelected = [NORTH]) then
      newCoords.x2 := newCoords.x1 - calcWidth
    else
      newCoords.x2 := newCoords.x1 + calcWidth;
  end
  else
  // Check if movement is only horizontal
  if ((fAnchorSelected = [EAST]) or (fAnchorSelected = [WEST])) then
  begin
    // Horizontal movement: keep current height
    if (fKeepAspectRatio) then
    begin
      // Calculate width
      calcWidth := newCoords.x2 - newCoords.x1;

      // Make sure we're not going out of bounds
      if (EAST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) > Bounds.Right) then
        begin
          calcWidth := Bounds.Right - newCoords.x1; // Limite width dimension
          newCoords.x2 := Bounds.Right;
        end;
      end;

      if (WEST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) < Bounds.Left) then
        begin
          calcWidth := -(newCoords.x1 - Bounds.Left); // Limite width dimension
          newCoords.x2 := Bounds.Left;
        end;
      end;

      // Calculate the new height based on the proportion of width
      calcHeight := Trunc(abs(calcWidth) * (fRatio.Vertical / fRatio.Horizontal));

      // Make sure we're not going out of bounds
      if (fAnchorSelected = [WEST]) then
      begin
        if ((newCoords.y1 - calcHeight) < Bounds.Top) then
        begin
          calcHeight := newCoords.y1 - Bounds.Top; // Limite height dimension
          newCoords.y2 := Bounds.Top;
          RecalculatesOtherAxis := True;
        end;
      end
      else
      begin
        if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
        begin
          calcHeight := Bounds.Bottom - newCoords.y1; // Limite height dimension
          newCoords.y2 := Bounds.Bottom;
          RecalculatesOtherAxis := True;
        end;
      end;

      // Apply calculated dimensions of height on width
      if (RecalculatesOtherAxis) then
      begin
        if (calcWidth > 0) then
          calcWidth := Trunc(calcHeight * (fRatio.Horizontal / fRatio.Vertical))
        else
          calcWidth := -Trunc(calcHeight * (fRatio.Horizontal / fRatio.Vertical));

        newCoords.x2 := newCoords.x1 + calcWidth;
      end;
    end
    else
    begin
      // Calculate width
      calcWidth := newCoords.x2 - newCoords.x1;

      // Make sure we're not going out of bounds
      if (EAST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) > Bounds.Right) then
        begin
          calcWidth := Bounds.Right - newCoords.x1; // Limite width dimension
          newCoords.x2 := Bounds.Right;
        end;
      end;

      if (WEST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) < Bounds.Left) then
        begin
          calcWidth := -(newCoords.x1 - Bounds.Left); // Limite width dimension
          newCoords.x2 := Bounds.Left;
        end;
      end;

      // Calculate height
      calcHeight := abs(fCropArea.Bottom - fCropArea.Top);
    end;

    if (fAnchorSelected = [WEST]) then
      newCoords.y2 := newCoords.y1 - calcHeight
    else
      newCoords.y2 := newCoords.y1 + calcHeight;
  end
  else
  begin
    // Diagonal movement
    if (fKeepAspectRatio) then
    begin
      // Calculate width
      calcWidth := newCoords.x2 - newCoords.x1;

      // Make sure we're not going out of bounds
      if (EAST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) > Bounds.Right) then
        begin
          calcWidth := Bounds.Right - newCoords.x1; // Limite width dimension
          newCoords.x2 := Bounds.Right;
        end;
      end;

      if (WEST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) < Bounds.Left) then
        begin
          calcWidth := -(newCoords.x1 - Bounds.Left); // Limite width dimension
          newCoords.x2 := Bounds.Left;
        end;
      end;

      // Calculate the new height based on the proportion of width
      if ((newCoords.y2 - newCoords.y1) > 0) then
        calcHeight := Trunc(abs(calcWidth) * (fRatio.Vertical / fRatio.Horizontal))
      else
        calcHeight := -Trunc(abs(calcWidth) * (fRatio.Vertical / fRatio.Horizontal));

      // Make sure we're not going out of bounds
      if (calcHeight > 0) then
      begin
        if (SOUTH in Direction) then
        begin
          if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
          begin
            calcHeight := Bounds.Bottom - newCoords.y1; // Limite height dimension
            newCoords.y2 := Bounds.Bottom;
            RecalculatesOtherAxis := True;
          end;
        end
        else
        begin
          if ((newCoords.y1 - calcHeight) < Bounds.Top) then
          begin
            calcHeight := newCoords.y1 - Bounds.Top; // Limite height dimension
            newCoords.y2 := Bounds.Top;
            RecalculatesOtherAxis := True;
          end;
        end;
      end
      else
      begin
        if (SOUTH in Direction) then
        begin
          if ((newCoords.y1 - calcHeight) > Bounds.Bottom) then
          begin
            calcHeight := newCoords.y1 - Bounds.Bottom; // Limite height dimension
            newCoords.y2 := Bounds.Bottom;
            RecalculatesOtherAxis := True;
          end;
        end
        else
        begin
          if ((newCoords.y1 + calcHeight) < Bounds.Top) then
          begin
            calcHeight := Bounds.Top - newCoords.y1; // Limite height dimension
            newCoords.y2 := Bounds.Top;
            RecalculatesOtherAxis := True;
          end;
        end;
      end;

      // Apply calculated dimensions of height on width
      if (RecalculatesOtherAxis) then
      begin
        if (calcWidth > 0) then
          calcWidth := Trunc(abs(calcHeight) * (fRatio.Horizontal / fRatio.Vertical))
        else
          calcWidth := -Trunc(abs(calcHeight) * (fRatio.Horizontal / fRatio.Vertical));

        newCoords.x2 := newCoords.x1 + calcWidth;
      end;
    end
    else
    begin
      // Calculate width
      calcWidth := newCoords.x2 - newCoords.x1;

      // Make sure we're not going out of bounds
      if (EAST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) > Bounds.Right) then
        begin
          calcWidth := Bounds.Right - newCoords.x1; // Limite width dimension
          newCoords.x2 := Bounds.Right;
        end;
      end;

      if (WEST in Direction) then
      begin
        if ((newCoords.x1 + calcWidth) < Bounds.Left) then
        begin
          calcWidth := -(newCoords.x1 - Bounds.Left); // Limite width dimension
          newCoords.x2 := Bounds.Left;
        end;
      end;

      // Calculate height
      calcHeight := newCoords.y2 - newCoords.y1;

      // Make sure we're not going out of bounds
      if (SOUTH in Direction) then
      begin
        if ((newCoords.y1 + calcHeight) > Bounds.Bottom) then
        begin
          calcHeight := Bounds.Bottom - newCoords.y1; // Limite height dimension
          newCoords.y2 := Bounds.Bottom;
        end;
      end;

      if (NORTH in Direction) then
      begin
        if ((newCoords.y1 + calcHeight) < Bounds.Top) then
        begin
          calcHeight := -(newCoords.y1 - Bounds.Top); // Limite height dimension
          newCoords.y2 := Bounds.Top;
        end;
      end;
    end;

    newCoords.x2 := newCoords.x1 + calcWidth;
    newCoords.y2 := newCoords.y1 + calcHeight;
  end;

  Result := newCoords;
end;

{ Calculate the maximun selection allowed                                      }
procedure TBGRAImageManipulation.CalcMaxSelection;
var
  ImageRect: TRect;
  newCoords: TCoord;
  Direction: TDirection;
  Bounds: TRect;
begin
  ImageRect := getImageRect(fImageBitmap);

  // Initiates coord
  with newCoords do
  begin
    x1 := 0;
    y1 := 0;

    x2 := ImageRect.Right - ImageRect.Left;
    y2 := ImageRect.Bottom - ImageRect.Top;
  end;

  // Determine direction
  Direction := getDirection(Point(newCoords.x1, newCoords.y1),
    Point(newCoords.x2, newCoords.y2));

  // Determines limite values
  with newCoords do
  begin
    x1 := 0;
    y1 := 0;
    x2 := ImageRect.Right - ImageRect.Left;
    y2 := ImageRect.Bottom - ImageRect.Top;
  end;
  Bounds := getImageRect(fResampledBitmap);

  // Apply the ratio
  newCoords := ApplyRatioToAxes(newCoords, Direction, Bounds);

  // Determines minimum value on both axes
  newCoords := ApplyDimRestriction(newCoords, Direction, Bounds);

  fCropArea := Rect(newCoords.x1, newCoords.y1, newCoords.x2, newCoords.y2);
end;

{ Calculate the Aspect Ratio for size limits}
procedure TBGRAImageManipulation.findSizeLimits;
var
  WorkRect: TRect;
begin
  // Find the working area of the component
  WorkRect := getWorkRect;

  with fSizeLimits do
  begin
    minWidth  := fAspectX;
    maxWidth  := WorkRect.Right - WorkRect.Left;
    minHeight := fAspectY;
    maxHeight := WorkRect.Bottom - WorkRect.Top;
  end;
end;

{ Get the direction of movement }
function TBGRAImageManipulation.getDirection(const Point1, Point2: TPoint): TDirection;
begin
  Result := [];

  if (Point1.X > Point2.X) then
    Result := Result + [WEST];

  if (Point1.X < Point2.X) then
    Result := Result + [EAST];

  if (Point1.Y > Point2.Y) then
    Result := Result + [NORTH];

  if (Point1.Y < Point2.Y) then
    Result := Result + [SOUTH];
end;

{ Calculate the Greatest Common Divisor (GCD) using the algorithm of Euclides }
function TBGRAImageManipulation.getGCD(Nr1, Nr2: longint): longint;
begin
  if Nr2 = 0 then
    Result := Nr1
  else
    Result := getGCD(Nr2, Nr1 mod Nr2);
end;

{ Calculate the Lowest Common Multiple (LCM) using the algorithm of Euclides }
function TBGRAImageManipulation.getLCM(Nr1, Nr2: longint): longint;
begin
  Result := (Nr1 * Nr2) div getGCD(Nr1, Nr2);
end;

{ Get image rectangle }
function TBGRAImageManipulation.getImageRect(Picture: TBGRABitmap): TRect;
var
  calcWidth, calcHeight, finalWidth, finalHeight, imageWidth, imageHeight: integer;
  WorkRect: TRect;
begin
  // Determine picture size
  imageWidth  := Picture.Width;
  imageHeight := Picture.Height;

  // Determine Work rectangle to final size
  WorkRect := getWorkRect;
  finalWidth := WorkRect.Right - WorkRect.Left;
  finalHeight := WorkRect.Bottom - WorkRect.Top;

  // Recalculate image dimensions
  calcHeight := (finalWidth * imageHeight) div imageWidth;
  calcWidth  := finalWidth;

  if (calcHeight > finalHeight) then
  begin
    calcHeight := finalHeight;
    calcWidth  := (calcHeight * imageWidth) div imageHeight;
  end;

  with Result do
  begin
    Left := 0;
    Top  := 0;
    Right := calcWidth;
    Bottom := calcHeight;
  end;
end;

{ Get work area rectangle }
function TBGRAImageManipulation.getWorkRect: TRect;
var
  // Number of units to remove from left, right, top, and bottom to get the
  // work rectangle
  Delta: integer;
begin
  // Start with the border size
  Delta := fBorderSize;

  // Get the coordinates of the control
  if (fVirtualScreen <> nil) then
    Result := Rect(0, 0, fVirtualScreen.Width, fVirtualScreen.Height)
  else
    Result := GetClientRect;

  // Remove the non-work areas from our work rectangle
  InflateRect(Result, -Delta, -Delta);
end;

{ Check if mouse is over any anchor }
function TBGRAImageManipulation.isOverAnchor(Point: TPoint; Corner: TPoint): boolean;
begin
  Result := ((Point.X >= (Corner.X - AnchorSize)) and
    (Point.X <= (Corner.X + AnchorSize)) and (Point.Y >= (Corner.Y - AnchorSize)) and
    (Point.Y <= (Corner.Y + AnchorSize)));
end;


 { ============================================================================ }
 { =====[ Component Definition ]=============================================== }
 { ============================================================================ }

constructor TBGRAImageManipulation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set default component values
  inherited Width := 320;
  inherited Height := 240;

  // Default property values
  fAnchorSize := 5;
  fAnchorSelected := [];
  fBorderSize := 2;
  fAspectRatio := '3:4';
  fAspectX := 3;
  fAspectY := 4;
  fKeepAspectRatio := True;

  // Default control values
  ControlStyle := ControlStyle + [csReplicatable];
  Cursor := crDefault;

  // Calculate the ratio
  fGCD := getGCD(fAspectX, fAspectY);


  // Determine the ratio of scale per axle
  with fRatio do
  begin
    Horizontal := fAspectX div fGCD;
    Vertical := fAspectY div fGCD;
  end;

  // Find size limits
  findSizeLimits;

  // Create the Image Bitmap
  fImageBitmap := TBGRABitmap.Create;

  // Create the Resampled Bitmap
  fResampledBitmap := TBGRABitmap.Create;

  // Create the Background
  fBackground := TBGRABitmap.Create(Width, Height);

  // Create render surface
  fVirtualScreen := TBGRABitmap.Create(Width, Height);

  // Force Render Struct
  RepaintBackground;
  Render;

  // Initialize crop area
  fCropArea := Rect(0, 0, 0, 0);
  fDeltaX := 0;
  fDeltaY := 0;
end;

destructor TBGRAImageManipulation.Destroy;
begin
  fImageBitmap.Free;
  fResampledBitmap.Free;
  fBackground.Free;
  fVirtualScreen.Free;
  inherited Destroy;
end;

procedure TBGRAImageManipulation.Invalidate;
begin
  inherited Invalidate;
end;

procedure TBGRAImageManipulation.Paint;
begin
  inherited Paint;
  fVirtualScreen.Draw(Canvas, 0, 0, True);
end;

{ This function repaint the background only when necessary to avoid unnecessary
  redraws. Contain a function called DrawCheckers that draws the Background like
  checkers game. Also included was a function that draws 3D effects changed to
  allow color changes. }
procedure TBGRAImageManipulation.RepaintBackground;

  procedure DrawCheckers(bmp: TBGRABitmap; ARect: TRect);
  const
    tx = 8;
    ty = 8;
  var
    xb, yb, xdest, ydest, nbx, nby: integer;
    oddColor, evenColor: TBGRAPixel;
  begin
    oddColor := BGRA(220, 220, 220);
    evenColor := BGRA(255, 255, 255);
    bmp.ClipRect := ARect;
    xdest := ARect.Left;
    nbx := ((ARect.Right - ARect.Left) + tx - 1) div tx;
    nby := ((ARect.Bottom - ARect.Top) + ty - 1) div ty;
    for xb := 0 to nbx - 1 do
    begin
      ydest := ARect.Top;
      for yb := 0 to nby - 1 do
      begin
        if odd(xb + yb) then
          bmp.FillRect(xdest, ydest, xdest + tx, ydest + ty, oddColor, dmSet)
        else
          bmp.FillRect(xdest, ydest, xdest + tx, ydest + ty, evenColor, dmSet);
        Inc(ydest, ty);
      end;
      Inc(xdest, tx);
    end;
    bmp.NoClip;
  end;

var
  Border: TRect;
  Grad: TBGRAGradientScanner;
begin
  // Resize background
  fBackground.SetSize(fVirtualScreen.Width, fVirtualScreen.Height);

  // Draw the outer bevel
  Border := Rect(0, 0, fVirtualScreen.Width, fVirtualScreen.Height);

  // Draw the rectangle around image
  if (fBorderSize > 2) then
  begin
    // Draw the border gradient
    Grad := TBGRAGradientScanner.Create(BGRA(245, 245, 245),
      BGRA(205, 204, 203), gtLinear, PointF(0, 0), PointF(0, fBackground.Height));
    fBackground.FillRect(0, 0, fBackground.Width, fBorderSize - 2, Grad, dmSet);
    fBackground.FillRect(0, fBorderSize - 2, fBorderSize - 2,
      fBackground.Height - fBorderSize + 2, Grad, dmSet);
    fBackground.FillRect(fBackground.Width - fBorderSize + 2, fBorderSize - 2,
      fBackground.Width, fBackground.Height - fBorderSize + 2,
      Grad, dmSet);
    fBackground.FillRect(0, fBackground.Height - fBorderSize + 2,
      fBackground.Width, fBackground.Height, Grad, dmSet);
    Grad.Free;
    InflateRect(Border, -(fBorderSize - 2), -(fBorderSize - 2));
  end;

  // Draw 3D border
  fBackground.CanvasBGRA.Frame3D(Border, 1, bvLowered,
    ColorToBGRA(ColorToRGB(clBtnHighlight)), ColorToBGRA(ColorToRGB(cl3DDkShadow)));
  fBackground.CanvasBGRA.Frame3D(Border, 1, bvLowered,
    ColorToBGRA(ColorToRGB(cl3DLight)), ColorToBGRA(ColorToRGB(clBtnShadow)));

  DrawCheckers(fBackground, Border);
end;

{ Resize the component, recalculating the proportions }
procedure TBGRAImageManipulation.Resize;

  function min(const Value: integer; const MinValue: integer): integer;
  begin
    if (Value < MinValue) then
      Result := MinValue
    else
      Result := Value;
  end;

var
  OriginalRect, SourceRect, DestinationRect: TRect;
  xRatio, yRatio:  double;
  ResampledBitmap: TBGRACustomBitmap;
begin
  inherited Resize;

  if (fVirtualScreen <> nil) then
  begin
    fVirtualScreen.SetSize(min(Self.Width, (fBorderSize * 2 + fAnchorSize + fMinWidth)),
      min(Self.Height, (fBorderSize * 2 + fAnchorSize + fMinHeight)));
    fVirtualScreen.InvalidateBitmap;

    // Resample the image
    if (not (fImageBitmap.Empty)) then
    begin
      // Get the resampled dimensions to scale image for draw in component
      DestinationRect := getImageRect(fImageBitmap);

      // Resize crop area
      if ((abs(fCropArea.Right - fCropArea.Left) > 0) and
        (abs(fCropArea.Bottom - fCropArea.Top) > 0)) then
      begin
        // Calculate scale from original size and destination size
        with OriginalRect do
        begin
          Left := 0;
          Right := fResampledBitmap.Width;
          Top := 0;
          Bottom := fResampledBitmap.Height;
        end;
        xRatio := fImageBitmap.Width / (OriginalRect.Right - OriginalRect.Left);
        yRatio := fImageBitmap.Height / (OriginalRect.Bottom - OriginalRect.Top);

        // Calculate source rectangle in original scale
        with SourceRect do
        begin
          Left := Round(fCropArea.Left * xRatio);
          Right := Round(fCropArea.Right * xRatio);
          Top := Round(fCropArea.Top * yRatio);
          Bottom := Round(fCropArea.Bottom * yRatio);
        end;

        // Calculate destination rectangle in new scale
        xRatio := fImageBitmap.Width / (DestinationRect.Right - DestinationRect.Left);
        yRatio := fImageBitmap.Height / (DestinationRect.Bottom - DestinationRect.Top);
        with fCropArea do
        begin
          Left := Round(SourceRect.Left / xRatio);
          ;
          Right := Round(SourceRect.Right / xRatio);
          Top := Round(SourceRect.Top / yRatio);
          Bottom := Round(SourceRect.Bottom / yRatio);
        end;
      end;

      // Recreate resampled bitmap
      try
        fResampledBitmap.Free;
        fResampledBitmap := TBGRABitmap.Create(DestinationRect.Right -
          DestinationRect.Left, DestinationRect.Bottom - DestinationRect.Top);
        ResampledBitmap  := fImageBitmap.Resample(DestinationRect.Right -
          DestinationRect.Left, DestinationRect.Bottom -
          DestinationRect.Top, rmFineResample);
        fResampledBitmap.PutImage(0, 0,
          ResampledBitmap,
          dmDrawWithTransparency);
      finally
        ResampledBitmap.Free;
      end;
    end;

    // Force Render Struct
    RepaintBackground;
    Render;
  end;

  Invalidate;
end;

{ Function responsible for rendering the content of the component, including
  the selection border and anchors. The selected area is painted with a
  different transparency level for easy viewing of what will be cut. }
procedure TBGRAImageManipulation.Render;
var
  WorkRect: TRect;
  Mask: TBGRABitmap;
  BorderColor, SelectColor, FillColor: TBGRAPixel;
begin
  // This procedure render main feature of engine

  // Render background
  fVirtualScreen.PutImage(0, 0,
    fBackground,
    dmDrawWithTransparency);

  // Render the image
  if (not (fImageBitmap.Empty)) then
  begin
    // Check if crop selection is empty
    if ((fCropArea.Left = fCropArea.Right) and
      (fCropArea.Top = fCropArea.Bottom)) then
      CalcMaxSelection;

    // Find the working area of the component
    WorkRect := getWorkRect;

    try
      // Draw image
      fVirtualScreen.PutImage(WorkRect.Left, WorkRect.Top,
        fResampledBitmap,
        dmDrawWithTransparency);

      // Render the selection background area
      BorderColor := BGRAWhite;
      FillColor := BGRA(0, 0, 0, 128);
      Mask := TBGRABitmap.Create(WorkRect.Right - WorkRect.Left,
        WorkRect.Bottom - WorkRect.Top, FillColor);
      Mask.EraseRectAntialias(fCropArea.Left + fDeltaX, fCropArea.Top + fDeltaY,
        fCropArea.Right + fDeltaX - 1,
        fCropArea.Bottom + fDeltaY - 1,
        255);

      // Draw a selection box
      with Rect(fCropArea.Left + fDeltaX, fCropArea.Top + fDeltaY,
          fCropArea.Right + fDeltaX - 1, fCropArea.Bottom + fDeltaY - 1) do
        Mask.DrawPolyLineAntialias([Point(Left, Top), Point(Right, Top),
          Point(Right, Bottom), Point(Left, Bottom), Point(Left, Top)],
          BorderColor, BGRAPixelTransparent, 1, False);

      // Draw anchors
      BorderColor := BGRABlack;
      SelectColor := BGRA(255, 255, 0, 255);
      FillColor := BGRA(255, 255, 0, 128);

      // NW
      Mask.Rectangle(fCropArea.Left + fDeltaX - fAnchorSize,
        fCropArea.Top + fDeltaY - fAnchorSize,
        fCropArea.Left + fDeltaX + fAnchorSize + 1,
        fCropArea.Top + fDeltaY + fAnchorSize + 1,
        BorderColor, FillColor, dmSet);

      // W
      Mask.Rectangle(fCropArea.Left + fDeltaX - fAnchorSize,
        (fCropArea.Top + fDeltaY + ((fCropArea.Bottom - fCropArea.Top) div 2)) -
        fAnchorSize,
        fCropArea.Left + fDeltaX + fAnchorSize + 1,
        (fCropArea.Top + fDeltaY + ((fCropArea.Bottom - fCropArea.Top) div 2)) +
        fAnchorSize + 1,
        BorderColor, FillColor, dmSet);

      // SW
      Mask.Rectangle(fCropArea.Left + fDeltaX - fAnchorSize,
        fCropArea.Bottom + fDeltaY - fAnchorSize - 1,
        fCropArea.Left + fDeltaX + fAnchorSize + 1,
        fCropArea.Bottom + fDeltaY + fAnchorSize,
        BorderColor, FillColor, dmSet);

      // S
      if ((fAnchorSelected = [NORTH]) and (fCropArea.Top < fCropArea.Bottom) and
        (fStartPoint.Y = fCropArea.Top)) or ((fAnchorSelected = [NORTH]) and
        (fCropArea.Top > fCropArea.Bottom) and (fStartPoint.Y = fCropArea.Top)) or
        ((fAnchorSelected = [SOUTH]) and (fCropArea.Top < fCropArea.Bottom) and
        (fStartPoint.Y = fCropArea.Top)) or ((fAnchorSelected = [SOUTH]) and
        (fCropArea.Top > fCropArea.Bottom) and (fStartPoint.Y = fCropArea.Top)) then
        Mask.Rectangle((fCropArea.Left + fDeltaX +
          ((fCropArea.Right - fCropArea.Left) div 2)) - fAnchorSize,
          fCropArea.Bottom + fDeltaY - fAnchorSize - 1,
          (fCropArea.Left + fDeltaX +
          ((fCropArea.Right - fCropArea.Left) div 2)) + fAnchorSize + 1,
          fCropArea.Bottom + fDeltaY + fAnchorSize,
          BorderColor, SelectColor, dmSet)
      else
        Mask.Rectangle((fCropArea.Left + fDeltaX +
          ((fCropArea.Right - fCropArea.Left) div 2)) - fAnchorSize,
          fCropArea.Bottom + fDeltaY - fAnchorSize - 1,
          (fCropArea.Left + fDeltaX +
          ((fCropArea.Right - fCropArea.Left) div 2)) + fAnchorSize + 1,
          fCropArea.Bottom + fDeltaY + fAnchorSize,
          BorderColor, FillColor, dmSet);

      // SE
      if ((fAnchorSelected = [NORTH, WEST]) and
        ((fCropArea.Left > fCropArea.Right) and (fCropArea.Top > fCropArea.Bottom))) or
        ((fAnchorSelected = [NORTH, WEST]) and
        ((fCropArea.Left < fCropArea.Right) and (fCropArea.Top < fCropArea.Bottom))) or
        ((fAnchorSelected = [NORTH, WEST]) and
        ((fCropArea.Left > fCropArea.Right) and (fCropArea.Top < fCropArea.Bottom))) or
        ((fAnchorSelected = [NORTH, WEST]) and
        ((fCropArea.Left < fCropArea.Right) and (fCropArea.Top > fCropArea.Bottom))) or
        ((fAnchorSelected = [NORTH, EAST]) and
        ((fCropArea.Left < fCropArea.Right) and (fCropArea.Top > fCropArea.Bottom))) or
        ((fAnchorSelected = [NORTH, EAST]) and
        ((fCropArea.Left > fCropArea.Right) and (fCropArea.Top < fCropArea.Bottom))) or
        ((fAnchorSelected = [NORTH, EAST]) and
        ((fCropArea.Left < fCropArea.Right) and (fCropArea.Top < fCropArea.Bottom))) or
        ((fAnchorSelected = [NORTH, EAST]) and
        ((fCropArea.Left > fCropArea.Right) and (fCropArea.Top > fCropArea.Bottom))) or
        ((fAnchorSelected = [SOUTH, EAST]) and
        ((fCropArea.Left > fCropArea.Right) and (fCropArea.Top > fCropArea.Bottom))) or
        ((fAnchorSelected = [SOUTH, EAST]) and
        ((fCropArea.Left < fCropArea.Right) and (fCropArea.Top < fCropArea.Bottom))) or
        ((fAnchorSelected = [SOUTH, EAST]) and
        ((fCropArea.Left > fCropArea.Right) and (fCropArea.Top < fCropArea.Bottom))) or
        ((fAnchorSelected = [SOUTH, EAST]) and
        ((fCropArea.Left < fCropArea.Right) and (fCropArea.Top > fCropArea.Bottom))) or
        ((fAnchorSelected = [SOUTH, WEST]) and
        ((fCropArea.Left > fCropArea.Right) and (fCropArea.Top < fCropArea.Bottom))) or
        ((fAnchorSelected = [SOUTH, WEST]) and
        ((fCropArea.Left < fCropArea.Right) and (fCropArea.Top > fCropArea.Bottom))) or
        ((fAnchorSelected = [SOUTH, WEST]) and
        ((fCropArea.Left > fCropArea.Right) and (fCropArea.Top > fCropArea.Bottom))) or
        ((fAnchorSelected = [SOUTH, WEST]) and
        ((fCropArea.Left < fCropArea.Right) and (fCropArea.Top < fCropArea.Bottom))) then
        Mask.Rectangle(fCropArea.Right + fDeltaX - fAnchorSize - 1,
          fCropArea.Bottom + fDeltaY - fAnchorSize - 1,
          fCropArea.Right + fDeltaX + fAnchorSize,
          fCropArea.Bottom + fDeltaY + fAnchorSize,
          BorderColor, SelectColor, dmSet)
      else
        Mask.Rectangle(fCropArea.Right + fDeltaX - fAnchorSize - 1,
          fCropArea.Bottom + fDeltaY - fAnchorSize - 1,
          fCropArea.Right + fDeltaX + fAnchorSize,
          fCropArea.Bottom + fDeltaY + fAnchorSize,
          BorderColor, FillColor, dmSet);

      // E
      if ((fAnchorSelected = [EAST]) and (fCropArea.Left < fCropArea.Right) and
        (fStartPoint.X = fCropArea.Left)) or ((fAnchorSelected = [EAST]) and
        (fCropArea.Left > fCropArea.Right) and (fStartPoint.X = fCropArea.Left)) or
        ((fAnchorSelected = [WEST]) and (fCropArea.Left < fCropArea.Right) and
        (fStartPoint.X = fCropArea.Left)) or ((fAnchorSelected = [WEST]) and
        (fCropArea.Left > fCropArea.Right) and (fStartPoint.X = fCropArea.Left)) then
        Mask.Rectangle(fCropArea.Right + fDeltaX - fAnchorSize - 1,
          (fCropArea.Top + fDeltaY + ((fCropArea.Bottom - fCropArea.Top) div 2)) -
          fAnchorSize,
          fCropArea.Right + fDeltaX + fAnchorSize,
          (fCropArea.Top + fDeltaY + ((fCropArea.Bottom - fCropArea.Top) div 2)) +
          fAnchorSize + 1,
          BorderColor, SelectColor, dmSet)
      else
        Mask.Rectangle(fCropArea.Right + fDeltaX - fAnchorSize - 1,
          (fCropArea.Top + fDeltaY + ((fCropArea.Bottom - fCropArea.Top) div 2)) -
          fAnchorSize,
          fCropArea.Right + fDeltaX + fAnchorSize,
          (fCropArea.Top + fDeltaY + ((fCropArea.Bottom - fCropArea.Top) div 2)) +
          fAnchorSize + 1,
          BorderColor, FillColor, dmSet);

      // NE
      Mask.Rectangle(fCropArea.Right + fDeltaX - fAnchorSize - 1,
        fCropArea.Top + fDeltaY - fAnchorSize,
        fCropArea.Right + fDeltaX + fAnchorSize,
        fCropArea.Top + fDeltaY + fAnchorSize + 1,
        BorderColor, FillColor, dmSet);

      // N
      Mask.Rectangle((fCropArea.Left + fDeltaX +
        ((fCropArea.Right - fCropArea.Left) div 2)) - fAnchorSize,
        fCropArea.Top + fDeltaY - fAnchorSize,
        (fCropArea.Left + fDeltaX + ((fCropArea.Right - fCropArea.Left) div 2)) +
        fAnchorSize + 1,
        fCropArea.Top + fDeltaY + fAnchorSize + 1,
        BorderColor, FillColor, dmSet);
    finally
      fVirtualScreen.PutImage(WorkRect.Left, WorkRect.Top,
        Mask,
        dmDrawWithTransparency);
      Mask.Free;
    end;
  end;
end;


 { ============================================================================ }
 { =====[ Properties Manipulation ]============================================ }
 { ============================================================================ }

function TBGRAImageManipulation.getAnchorSize: byte;
begin
  Result := fAnchorSize * 2 + 1;
end;

procedure TBGRAImageManipulation.setAnchorSize(const Value: byte);
const
  MinSize = 3;
  MaxSize = 9;
begin
  if (Value <> getAnchorSize) then
  begin
    if (Value < MinSize) then
    begin
      raise ERangeError.CreateFmt(SAnchorSizeIsTooSmall,
        [Value, MinSize, MaxSize]);
    end
    else
    begin
      if (Value > MaxSize) then
      begin
        raise ERangeError.CreateFmt(SAnchorSizeIsTooLarge,
          [Value, MinSize, MaxSize]);
      end
      else
      begin
        if ((Value mod 2) = 0) then
        begin
          raise EInvalidArgument.CreateFmt(SAnchorSizeIsNotOdd, [Value]);
        end
        else
        begin
          fAnchorSize := (Value div 2);
          Render;
          Refresh;
        end;
      end;
    end;
  end;
end;

function TBGRAImageManipulation.getEmpty: boolean;
begin
  Result := fImageBitmap.Empty;
end;

function TBGRAImageManipulation.getBitmap: TBGRABitmap;
var
  ResampledBitmap: TBGRACustomBitmap;
  CropBitmap:  TBGRABitmap;
  FinalBitmap: TBGRABitmap;
  xRatio, yRatio: double;
  OriginalRect, SourceRect, DestRect: Trect;
begin
  if not (fImageBitmap.Empty) then
  begin
    try
      // Calculate scale from original size and destination size
      OriginalRect := getImageRect(fImageBitmap);
      xRatio := fImageBitmap.Width / (OriginalRect.Right - OriginalRect.Left);
      yRatio := fImageBitmap.Height / (OriginalRect.Bottom - OriginalRect.Top);

      // Calculate source rectangle in original scale
      with SourceRect do
      begin
        Left := Round(fCropArea.Left * xRatio);
        Right := Round(fCropArea.Right * xRatio);
        Top := Round(fCropArea.Top * yRatio);
        Bottom := Round(fCropArea.Bottom * yRatio);
      end;

      // Calculate destination rectangle in original scale
      with DestRect do
      begin
        Left := 0;
        Right := SourceRect.Right - SourceRect.Left;
        Top := 0;
        Bottom := SourceRect.Bottom - SourceRect.Top;
      end;

      // Create a new bitmap for cropped region in original scale
      CropBitmap := TBGRABitmap.Create(SourceRect.Right - SourceRect.Left,
        SourceRect.Bottom - SourceRect.Top);

      // Get the cropped image on selected region in original scale
      CropBitmap.Canvas.CopyRect(DestRect, fImageBitmap.Canvas, SourceRect);

      // Create bitmap to put image on final scale
      FinalBitmap := TBGRABitmap.Create(fCropArea.Right - fCropArea.Left,
        fCropArea.Bottom - fCropArea.Top);

      // Resize the cropped image to final scale
      try
        ResampledBitmap := CropBitmap.Resample(fCropArea.Right -
          fCropArea.Left, fCropArea.Bottom - fCropArea.Top, rmFineResample);
        FinalBitmap.PutImage(0, 0,
          ResampledBitmap,
          dmDrawWithTransparency);
      finally
        ResampledBitmap.Free
      end;
    finally
      Result := FinalBitmap;

      CropBitmap.Free;
    end;
  end
  else
  begin
    Result := fImageBitmap;
  end;
end;

procedure TBGRAImageManipulation.setBitmap(const Value: TBGRABitmap);

  function min(const Value: integer; const MinValue: integer): integer;
  begin
    if (Value < MinValue) then
      Result := MinValue
    else
      Result := Value;
  end;

var
  SourceRect, OriginalRect, DestinationRect: TRect;
  ResampledBitmap: TBGRACustomBitmap;
  xRatio, yRatio:  double;
begin
  if (Value <> fImageBitmap) then
  begin
    try
      // Clear actual image
      fImageBitmap.Free;
      fImageBitmap := TBGRABitmap.Create(Value.Width, Value.Height);

      // Prevent empty image
      if Value.Empty then
        exit;

      // Prevent null image
      if (Value.Width = 0) or (Value.Height = 0) then
        exit;

      // Associate the new bitmap
      fImageBitmap.Assign(Value);

      // Get the resampled dimensions to scale image for draw in component
      DestinationRect := getImageRect(fImageBitmap);

      // Recreate resampled bitmap
      try
        fResampledBitmap.Free;
        fResampledBitmap := TBGRABitmap.Create(DestinationRect.Right -
          DestinationRect.Left, DestinationRect.Bottom - DestinationRect.Top);
        ResampledBitmap  := fImageBitmap.Resample(DestinationRect.Right -
          DestinationRect.Left, DestinationRect.Bottom -
          DestinationRect.Top, rmFineResample);
        fResampledBitmap.PutImage(0, 0,
          ResampledBitmap,
          dmDrawWithTransparency);
      finally
        ResampledBitmap.Free;
      end;

      // Resize crop area
      if ((abs(fCropArea.Right - fCropArea.Left) > 0) and
        (abs(fCropArea.Bottom - fCropArea.Top) > 0)) then
      begin
        // Calculate scale from original size and destination size
        with OriginalRect do
        begin
          Left := 0;
          Right := fResampledBitmap.Width;
          Top := 0;
          Bottom := fResampledBitmap.Height;
        end;
        xRatio := fImageBitmap.Width / (OriginalRect.Right - OriginalRect.Left);
        yRatio := fImageBitmap.Height / (OriginalRect.Bottom - OriginalRect.Top);

        // Calculate source rectangle in original scale
        with SourceRect do
        begin
          Left := Round(fCropArea.Left * xRatio);
          Right := Round(fCropArea.Right * xRatio);
          Top := Round(fCropArea.Top * yRatio);
          Bottom := Round(fCropArea.Bottom * yRatio);
        end;

        // Calculate destination rectangle in new scale
        xRatio := fImageBitmap.Width / (DestinationRect.Right - DestinationRect.Left);
        yRatio := fImageBitmap.Height / (DestinationRect.Bottom - DestinationRect.Top);
        with fCropArea do
        begin
          Left := Round(SourceRect.Left / xRatio);
          ;
          Right := Round(SourceRect.Right / xRatio);
          Top := Round(SourceRect.Top / yRatio);
          Bottom := Round(SourceRect.Bottom / yRatio);
        end;
      end
      else
      begin
        // Calculates maximum crop selection
        CalcMaxSelection;
      end;
    finally
      // Force Render Struct
      Render;
      inherited Invalidate;
    end;
  end;
end;

procedure TBGRAImageManipulation.rotateLeft;
var
  SourceRect, OriginalRect, DestinationRect: TRect;
  TempBitmap, ResampledBitmap: TBGRACustomBitmap;
  xRatio, yRatio: double;
begin
  try
    // Prevent empty image
    if fImageBitmap.Empty then
      exit;

    // Rotate bitmap
    TempBitmap := fImageBitmap.RotateCCW;
    fImageBitmap.Assign(TempBitmap);

    // Get the resampled dimensions to scale image for draw in component
    DestinationRect := getImageRect(fImageBitmap);

    // Recreate resampled bitmap
    try
      fResampledBitmap.Free;
      fResampledBitmap := TBGRABitmap.Create(DestinationRect.Right -
        DestinationRect.Left, DestinationRect.Bottom - DestinationRect.Top);
      ResampledBitmap  := fImageBitmap.Resample(DestinationRect.Right -
        DestinationRect.Left, DestinationRect.Bottom - DestinationRect.Top,
        rmFineResample);
      fResampledBitmap.PutImage(0, 0,
        ResampledBitmap,
        dmDrawWithTransparency);
    finally
      ResampledBitmap.Free;
    end;

    // Resize crop area
    if ((abs(fCropArea.Right - fCropArea.Left) > 0) and
      (abs(fCropArea.Bottom - fCropArea.Top) > 0)) then
    begin
      // Calculate scale from original size and destination size
      with OriginalRect do
      begin
        Left := 0;
        Right := fResampledBitmap.Width;
        Top := 0;
        Bottom := fResampledBitmap.Height;
      end;
      xRatio := fImageBitmap.Width / (OriginalRect.Right - OriginalRect.Left);
      yRatio := fImageBitmap.Height / (OriginalRect.Bottom - OriginalRect.Top);

      // Calculate source rectangle in original scale
      with SourceRect do
      begin
        Left := Round(fCropArea.Left * xRatio);
        Right := Round(fCropArea.Right * xRatio);
        Top := Round(fCropArea.Top * yRatio);
        Bottom := Round(fCropArea.Bottom * yRatio);
      end;

      // Calculate destination rectangle in new scale
      xRatio := fImageBitmap.Width / (DestinationRect.Right - DestinationRect.Left);
      yRatio := fImageBitmap.Height / (DestinationRect.Bottom - DestinationRect.Top);
      with fCropArea do
      begin
        Left := Round(SourceRect.Left / xRatio);
        ;
        Right := Round(SourceRect.Right / xRatio);
        Top := Round(SourceRect.Top / yRatio);
        Bottom := Round(SourceRect.Bottom / yRatio);
      end;
    end
    else
    begin
      // Calculates maximum crop selection
      CalcMaxSelection;
    end;
  finally
    // Force Render Struct
    Render;
    inherited Invalidate;
    TempBitmap.Free;
  end;
end;

procedure TBGRAImageManipulation.rotateRight;
var
  SourceRect, OriginalRect, DestinationRect: TRect;
  TempBitmap, ResampledBitmap: TBGRACustomBitmap;
  xRatio, yRatio: double;
begin
  try
    // Prevent empty image
    if fImageBitmap.Empty then
      exit;

    // Rotate bitmap
    TempBitmap := fImageBitmap.RotateCW;
    fImageBitmap.Assign(TempBitmap);

    // Get the resampled dimensions to scale image for draw in component
    DestinationRect := getImageRect(fImageBitmap);

    // Recreate resampled bitmap
    try
      fResampledBitmap.Free;
      fResampledBitmap := TBGRABitmap.Create(DestinationRect.Right -
        DestinationRect.Left, DestinationRect.Bottom - DestinationRect.Top);
      ResampledBitmap  := fImageBitmap.Resample(DestinationRect.Right -
        DestinationRect.Left, DestinationRect.Bottom - DestinationRect.Top,
        rmFineResample);
      fResampledBitmap.PutImage(0, 0,
        ResampledBitmap,
        dmDrawWithTransparency);
    finally
      ResampledBitmap.Free;
    end;

    // Resize crop area
    if ((abs(fCropArea.Right - fCropArea.Left) > 0) and
      (abs(fCropArea.Bottom - fCropArea.Top) > 0)) then
    begin
      // Calculate scale from original size and destination size
      with OriginalRect do
      begin
        Left := 0;
        Right := fResampledBitmap.Width;
        Top := 0;
        Bottom := fResampledBitmap.Height;
      end;
      xRatio := fImageBitmap.Width / (OriginalRect.Right - OriginalRect.Left);
      yRatio := fImageBitmap.Height / (OriginalRect.Bottom - OriginalRect.Top);

      // Calculate source rectangle in original scale
      with SourceRect do
      begin
        Left := Round(fCropArea.Left * xRatio);
        Right := Round(fCropArea.Right * xRatio);
        Top := Round(fCropArea.Top * yRatio);
        Bottom := Round(fCropArea.Bottom * yRatio);
      end;

      // Calculate destination rectangle in new scale
      xRatio := fImageBitmap.Width / (DestinationRect.Right - DestinationRect.Left);
      yRatio := fImageBitmap.Height / (DestinationRect.Bottom - DestinationRect.Top);
      with fCropArea do
      begin
        Left := Round(SourceRect.Left / xRatio);
        ;
        Right := Round(SourceRect.Right / xRatio);
        Top := Round(SourceRect.Top / yRatio);
        Bottom := Round(SourceRect.Bottom / yRatio);
      end;
    end
    else
    begin
      // Calculates maximum crop selection
      CalcMaxSelection;
    end;
  finally
    // Force Render Struct
    Render;
    inherited Invalidate;
    TempBitmap.Free;
  end;
end;

procedure TBGRAImageManipulation.setBorderSize(const Value: byte);
const
  MinSize = 2;
  MaxSize = 10;
begin
  if (Value <> fBorderSize) then
  begin
    if (Value < MinSize) then
    begin
      raise ERangeError.CreateFmt(SBorderSizeIsTooSmall,
        [Value, MinSize, MaxSize]);
    end
    else
    begin
      if (Value > MaxSize) then
      begin
        raise ERangeError.CreateFmt(SBorderSizeIsTooLarge,
          [Value, MinSize, MaxSize]);
      end
      else
      begin
        fBorderSize := Value;

        Resize;
      end;
    end;
  end;
end;

procedure TBGRAImageManipulation.setKeepAspectRatio(const Value: boolean);
begin
  if (Value <> fKeepAspectRatio) then
  begin
    fKeepAspectRatio := Value;
    if (fKeepAspectRatio) then
    begin
      if (not (fImageBitmap.Empty)) then
      begin
        // Check if crop selection is not empty
        if not ((fCropArea.Left = fCropArea.Right) and
          (fCropArea.Top = fCropArea.Bottom)) then
          CalcMaxSelection;
        Render;
      end;
    end;

    Refresh;
  end;
end;

function TBGRAImageManipulation.getAspectRatioFromImage(
  const Value: TBGRABitmap): string;
var
  GCD: integer;
begin
  GCD := getGCD(Value.Width, Value.Height);

  Result := IntToStr(Value.Width div GCD) + ':' + IntToStr(Value.Height div GCD);
end;

procedure TBGRAImageManipulation.setAspectRatio(const Value: string);
const
  ValidChars = ['0'..'9', ':'];
var
  Count, XValue, YValue: integer;
  AspectRatioText: string;
begin
  if (Value <> fAspectRatio) then
  begin
    // Check if value contain a valid string
    if ((pos(':', Value) > 0) and (pos(':', Value) < Length(Value))) then
    begin
      // Check if value is valid
      XValue := 0;
      YValue := 0;
      AspectRatioText := '';
      for Count := 1 to Length(Value) do
      begin
        if (Value[Count] in ValidChars) then
        begin
          if ((Value[Count] = ':') and (Length(AspectRatioText) > 0) and
            (XValue = 0)) then
          begin
            XValue := StrToInt(AspectRatioText);
          end;

          AspectRatioText := AspectRatioText + Value[Count];
        end
        else
        begin
          // Value contain invalid characters
          raise EInvalidArgument.CreateFmt(SAspectRatioIsNotValid, [Value]);
        end;
      end;
      YValue := StrToInt(Copy(AspectRatioText, Pos(':', AspectRatioText) + 1,
        Length(AspectRatioText)));
    end
    else
    begin
      // Value contain invalid characters
      raise EInvalidArgument.CreateFmt(SAspectRatioIsNotValid, [Value]);
    end;

    // Set new Aspect Ratio
    fAspectRatio := AspectRatioText;
    fAspectX := XValue;
    fAspectY := YValue;

    // Calculate the ratio
    fGCD := getGCD(fAspectX, fAspectY);

    // Determine the ratio of scale per axle
    with fRatio do
    begin
      Horizontal := fAspectX div fGCD;
      Vertical := fAspectY div fGCD;
    end;

    // Set minimun size
    if ((fRatio.Horizontal < fAnchorSize + 10) or
      (fRatio.Vertical < fAnchorSize + 10)) then
    begin
      fMinWidth  := fRatio.Horizontal * 10;
      fMinHeight := fRatio.Vertical * 10;
    end
    else
    begin
      fMinWidth  := fRatio.Horizontal;
      fMinHeight := fRatio.Vertical;
    end;

    if (not (fImageBitmap.Empty)) then
    begin
      // Check if crop selection is not empty
      if not ((fCropArea.Left = fCropArea.Right) and
        (fCropArea.Top = fCropArea.Bottom)) then
        CalcMaxSelection;
      Render;
    end;

    Invalidate;
  end;
end;

procedure TBGRAImageManipulation.setMinHeight(const Value: integer);
begin
  if (Value <> fMinHeight) then
  begin
    if (Value < fSizeLimits.minHeight) then
    begin
      fMinHeight := fSizeLimits.minHeight;
    end
    else
    begin
      if (Value > fSizeLimits.maxHeight) then
      begin
        fMinHeight := fSizeLimits.maxHeight;
      end
      else
      begin
        fMinHeight := Value;
      end;
    end;

    if (fKeepAspectRatio) then
    begin
      // Recalculates the width value based on height
      fMinWidth := Trunc(fMinHeight * (fRatio.Horizontal / fRatio.Vertical));
    end;

    Render;
    Invalidate;
  end;
end;

procedure TBGRAImageManipulation.setMinWidth(const Value: integer);
begin
  if (Value <> fMinWidth) then
  begin
    if (Value < fSizeLimits.minWidth) then
    begin
      fMinWidth := fSizeLimits.minWidth;
    end
    else
    begin
      if (Value > fSizeLimits.maxWidth) then
      begin
        fMinWidth := fSizeLimits.maxWidth;
      end
      else
      begin
        fMinWidth := Value;
      end;
    end;

    if (fKeepAspectRatio) then
    begin
      // Recalculates the height value based on width
      fMinHeight := Trunc(fMinWidth * (fRatio.Vertical / fRatio.Horizontal));
    end;

    Render;
    Invalidate;
  end;
end;


 { ============================================================================ }
 { =====[ Event Control ]====================================================== }
 { ============================================================================ }

procedure TBGRAImageManipulation.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  WorkRect: TRect;
  overControl: boolean;
begin
  // Call the inherited MouseDown() procedure
  inherited MouseDown(Button, Shift, X, Y);

  // Find the working area of the control
  WorkRect := getWorkRect;

  // See if the mouse is inside the pressable part of the control
  overControl := ((X >= WorkRect.Left) and (X <= WorkRect.Right) and
    (Y >= WorkRect.Top) and (Y <= WorkRect.Bottom));

  // If over control
  if ((overControl) and (Button = mbLeft) and (not (ssDouble in Shift))) then
  begin
    // If this was the left mouse button and nor double click
    fMouseCaught := True;
    fStartPoint  := Point(X - WorkRect.Left, Y - WorkRect.Top);

    if (fAnchorSelected <> []) then
    begin
      // Resize the cropping area from cornes

      // Get the coordinate corresponding to the opposite quadrant and
      // set into fStartPoint
      if ((fAnchorSelected = [NORTH]) or (fAnchorSelected = [WEST]) or
        (fAnchorSelected = [NORTH, WEST])) then
        fStartPoint := Point(fCropArea.Right, fCropArea.Bottom);

      if (fAnchorSelected = [SOUTH, WEST]) then
        fStartPoint := Point(fCropArea.Right, fCropArea.Top);

      if ((fAnchorSelected = [SOUTH]) or (fAnchorSelected = [EAST]) or
        (fAnchorSelected = [SOUTH, EAST])) then
        fStartPoint := Point(fCropArea.Left, fCropArea.Top);

      if (fAnchorSelected = [NORTH, EAST]) then
        fStartPoint := Point(fCropArea.Left, fCropArea.Bottom);
    end;
  end;
end;

procedure TBGRAImageManipulation.MouseMove(Shift: TShiftState; X, Y: integer);
var
  needRepaint: boolean;
  WorkRect: TRect;
  newCoords: TCoord;
  Direction: TDirection;
  Bounds: TRect;
  overControl: boolean;
begin
  // Call the inherited MouseMove() procedure
  inherited MouseMove(Shift, X, Y);

  // Set default cursor
  Cursor := crDefault;

  // Assume we don't need to repaint the control
  needRepaint := False;

  // Find the working area of the component
  WorkRect := GetWorkRect;

  // See if the mouse is inside the pressable part of the control
  overControl := ((X >= WorkRect.Left) and (X <= WorkRect.Right) and
    (Y >= WorkRect.Top) and (Y <= WorkRect.Bottom));

  // If image empty
  if (fImageBitmap.Empty) then
    exit;

  // If the mouse was originally clicked on the control
  if (fMouseCaught) then
  begin
    // If no anchor selected
    if (fAnchorSelected = []) then
    begin
      // Starts a new selection of cropping area
      try
        Cursor := crCross;
        fEndPoint := Point(X - WorkRect.Left, Y - WorkRect.Top);

        // Copy coord
        with newCoords do
        begin
          x1 := fStartPoint.X;
          y1 := fStartPoint.Y;

          x2 := fEndPoint.X;
          y2 := fEndPoint.Y;
        end;

        // Determine direction
        Direction := getDirection(fStartPoint, fEndPoint);

        // Determines limite values
        Bounds := getImageRect(fResampledBitmap);

        // Apply the ratio, if necessary
        newCoords := ApplyRatioToAxes(newCoords, Direction, Bounds);

        // Determines minimum value on both axes
        newCoords := ApplyDimRestriction(newCoords, Direction, Bounds);

        fCropArea := Rect(newCoords.x1, newCoords.y1, newCoords.x2, newCoords.y2);
      finally
        needRepaint := True;
      end;
    end
    else
    begin
      // Get the actual point
      fEndPoint := Point(X - WorkRect.Left, Y - WorkRect.Top);

      // Check what the anchor was dragged
      if (fAnchorSelected = [NORTH, SOUTH, EAST, WEST]) then
      begin
        Cursor := crSizeAll;

        // Move the cropping area
        try
          // Gets the offset
          fDeltaX := fEndPoint.X - fStartPoint.X;
          fDeltaY := fEndPoint.Y - fStartPoint.Y;

          // Determines limite values
          Bounds := getImageRect(fResampledBitmap);

          if ((fCropArea.Left + fDeltaX) < Bounds.Left) then
          begin
            fDeltaX := fDeltaX + Abs(fCropArea.Left + fDeltaX);
          end;

          if ((fCropArea.Right + fDeltaX) > Bounds.Right) then
          begin
            fDeltaX := fDeltaX - Abs(fCropArea.Right + fDeltaX) + Bounds.Right;
          end;

          if ((fCropArea.Top + fDeltaY) < Bounds.Top) then
          begin
            fDeltaY := fDeltaY + Abs(fCropArea.Top + fDeltaY);
          end;

          if ((fCropArea.Bottom + fDeltaY) > Bounds.Bottom) then
          begin
            fDeltaY := fDeltaY - Abs(fCropArea.Bottom + fDeltaY) + Bounds.Bottom;
          end;
        finally
          needRepaint := True;
        end;
      end;

      if ((fAnchorSelected = [NORTH]) or (fAnchorSelected = [SOUTH]) or
        (fAnchorSelected = [EAST]) or (fAnchorSelected = [WEST]) or
        (fAnchorSelected = [NORTH, WEST]) or (fAnchorSelected = [SOUTH, WEST]) or
        (fAnchorSelected = [SOUTH, EAST]) or (fAnchorSelected =
        [NORTH, EAST])) then
      begin
        // Resize the cropping area
        try
          if ((fAnchorSelected = [NORTH]) or (fAnchorSelected = [SOUTH])) then
            Cursor := crSizeNS
          else
          if ((fAnchorSelected = [WEST]) or (fAnchorSelected = [EAST])) then
            Cursor := crSizeWE
          else
          if ((fAnchorSelected = [NORTH, WEST]) or
            (fAnchorSelected = [SOUTH, EAST])) then
            Cursor := crSizeNWSE
          else
            Cursor := crSizeNESW;

          // Copy coord
          with newCoords do
          begin
            x1 := fStartPoint.X;
            y1 := fStartPoint.Y;

            if (fAnchorSelected = [NORTH]) then
            begin
              x2 := fEndPoint.X - Abs(fCropArea.Right - fCropArea.Left) div 2;
              y2 := fEndPoint.Y;
            end
            else if (fAnchorSelected = [SOUTH]) then
            begin
              x2 := fEndPoint.X + Abs(fCropArea.Right - fCropArea.Left) div 2;
              y2 := fEndPoint.Y;
            end
            else if (fAnchorSelected = [EAST]) then
            begin
              x2 := fEndPoint.X;
              y2 := fEndPoint.Y + Abs(fCropArea.Bottom - fCropArea.Top) div 2;
            end
            else if (fAnchorSelected = [WEST]) then
            begin
              x2 := fEndPoint.X;
              y2 := fEndPoint.Y - Abs(fCropArea.Bottom - fCropArea.Top) div 2;
            end
            else
            begin
              x2 := fEndPoint.X;
              y2 := fEndPoint.Y;
            end;
          end;

          // Determine direction
          Direction := getDirection(fStartPoint, fEndPoint);

          // Determines limite values
          Bounds := getImageRect(fResampledBitmap);

          // Apply the ratio, if necessary
          newCoords := ApplyRatioToAxes(newCoords, Direction, Bounds);

          // Determines minimum value on both axes
          newCoords := ApplyDimRestriction(newCoords, Direction, Bounds);

          fCropArea := Rect(newCoords.x1, newCoords.y1, newCoords.x2, newCoords.y2);
        finally
          needRepaint := True;
        end;
      end;
    end;
  end
  else
  begin
    // If the mouse is just moving over the control, and wasn't originally click
    // in the control
    if (overControl) then
    begin
      // Mouse is inside the pressable part of the control
      Cursor := crCross;
      fAnchorSelected := [];
      fEndPoint := Point(X - WorkRect.Left, Y - WorkRect.Top);

      // Verifies that is positioned on an anchor
      // NW
      if (isOverAnchor(fEndPoint, fCropArea.TopLeft)) then
      begin
        fAnchorSelected := [NORTH, WEST];
        Cursor := crSizeNWSE;
      end;

      // W
      if (isOverAnchor(fEndPoint, Point(fCropArea.Left, fCropArea.Top +
        (fCropArea.Bottom - fCropArea.Top) div 2))) then
      begin
        fAnchorSelected := [WEST];
        Cursor := crSizeWE;
      end;

      // SW
      if (isOverAnchor(fEndPoint, Point(fCropArea.Left, fCropArea.Bottom))) then
      begin
        fAnchorSelected := [SOUTH, WEST];
        Cursor := crSizeNESW;
      end;

      // S
      if (isOverAnchor(fEndPoint, Point(fCropArea.Left +
        ((fCropArea.Right - fCropArea.Left) div 2), fCropArea.Bottom))) then
      begin
        fAnchorSelected := [SOUTH];
        Cursor := crSizeNS;
      end;

      // SE
      if (isOverAnchor(fEndPoint, fCropArea.BottomRight)) then
      begin
        fAnchorSelected := [SOUTH, EAST];
        Cursor := crSizeNWSE;
      end;

      // E
      if (isOverAnchor(fEndPoint, Point(fCropArea.Right, fCropArea.Top +
        ((fCropArea.Bottom - fCropArea.Top) div 2)))) then
      begin
        fAnchorSelected := [EAST];
        Cursor := crSizeWE;
      end;

      // NE
      if (isOverAnchor(fEndPoint, Point(fCropArea.Right, fCropArea.Top))) then
      begin
        fAnchorSelected := [NORTH, EAST];
        Cursor := crSizeNESW;
      end;

      // N
      if (isOverAnchor(fEndPoint, Point(fCropArea.Left +
        ((fCropArea.Right - fCropArea.Left) div 2), fCropArea.Top))) then
      begin
        fAnchorSelected := [NORTH];
        Cursor := crSizeNS;
      end;

      // Verifies that is positioned on a cropping area
      if (fAnchorSelected = []) then
      begin
        if ((fEndPoint.X >= fCropArea.Left) and (fEndPoint.X <= fCropArea.Right) and
          (fEndPoint.Y >= fCropArea.Top) and (fEndPoint.Y <= fCropArea.Bottom)) then
        begin
          fAnchorSelected := [NORTH, SOUTH, EAST, WEST];
          Cursor := crSizeAll;
        end;
      end;
    end;
  end;

  // If we need to repaint
  if needRepaint then
  begin
    // Invalidate the control for repainting
    Render;
    Refresh;
  end;
end;

procedure TBGRAImageManipulation.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  needRepaint: boolean;
  temp: integer;
begin
  // Call the inherited MouseUp() procedure
  inherited MouseUp(Button, Shift, X, Y);

  // Assume we don't need to repaint the control
  needRepaint := False;

  // If the mouse was originally clicked over the control
  if (fMouseCaught) then
  begin
    // Show that the mouse is no longer caught
    fMouseCaught := False;

    // Check what the anchor was dragged
    if (fAnchorSelected = [NORTH, SOUTH, EAST, WEST]) then
    begin
      // Move the cropping area
      try
        OffsetRect(fCropArea, fDeltaX, fDeltaY);
        fDeltaX := 0;
        fDeltaY := 0;
      finally
        needRepaint := True;
      end;
    end
    else
    begin
      // Ends a new selection of cropping area
      if (fCropArea.Left > fCropArea.Right) then
      begin
        // Swap left and right coordinates
        temp := fCropArea.Left;
        fCropArea.Left := fCropArea.Right;
        fCropArea.Right := temp;
      end;

      if (fCropArea.Top > fCropArea.Bottom) then
      begin
        // Swap left and right coordinates
        temp := fCropArea.Top;
        fCropArea.Top := fCropArea.Bottom;
        fCropArea.Bottom := temp;
      end;

      needRepaint := True;
    end;

    fAnchorSelected := [];
  end;

  // If we need to repaint
  if needRepaint then
  begin
    // Invalidate the control for repainting
    Render;
    Refresh;
  end;
end;


 { ============================================================================ }
 { =====[ Register Function ]================================================== }
 { ============================================================================ }

procedure Register;
begin
  {$I BGRAImageManipulation_icon.lrs}
  RegisterComponents('BGRA Controls', [TBGRAImageManipulation]);
end;

end.
