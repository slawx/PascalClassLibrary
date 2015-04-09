unit BGRADithering;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAFilters, BGRAPalette, BGRABitmapTypes;

type

  { TDitheringTask }

  TDitheringTask = class(TFilterTask)
  protected
    FBounds: TRect;
    FIgnoreAlpha: boolean;
    FPalette: TBGRACustomApproxPalette;
  public
    constructor Create(bmp: TBGRACustomBitmap; APalette: TBGRACustomApproxPalette; AInPlace: boolean; AIgnoreAlpha: boolean; ABounds: TRect); overload;
    constructor Create(bmp: TBGRACustomBitmap; APalette: TBGRACustomApproxPalette; AInPlace: boolean; AIgnoreAlpha: boolean); overload;
  end;

  { TNearestColorTask }

  TNearestColorTask = class(TDitheringTask)
  protected
    procedure DoExecute; override;
  end;

  { TFloydSteinbergDitheringTask }

  TFloydSteinbergDitheringTask = class(TDitheringTask)
  protected
    procedure DoExecute; override;
  end;

function CreateDitheringTask(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; APalette: TBGRACustomApproxPalette;
  AIgnoreAlpha: boolean): TDitheringTask; overload;
function CreateDitheringTask(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; APalette: TBGRACustomApproxPalette;
  AIgnoreAlpha: boolean; ABounds: TRect): TDitheringTask; overload;

implementation

function AbsRGBADiff(const c1, c2: TExpandedPixel): NativeInt;
begin
  result := abs(c1.alpha-c2.alpha);
  result += abs(c1.red-c2.red);
  result += abs(c1.green-c2.green);
  result += abs(c1.blue-c2.blue);
end;

function CreateDitheringTask(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; APalette: TBGRACustomApproxPalette;
  AIgnoreAlpha: boolean): TDitheringTask;
begin
  result := CreateDitheringTask(AAlgorithm, ABitmap, APalette, AIgnoreAlpha, rect(0,0,ABitmap.width, ABitmap.Height));
end;

function CreateDitheringTask(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; APalette: TBGRACustomApproxPalette;
  AIgnoreAlpha: boolean; ABounds: TRect): TDitheringTask;
begin
  result := nil;
  case AAlgorithm of
    daNearestNeighbor: result := TNearestColorTask.Create(ABitmap, APalette, False, AIgnoreAlpha, ABounds);
    daFloydSteinberg: result := TFloydSteinbergDitheringTask.Create(ABitmap, APalette, False, AIgnoreAlpha, ABounds);
    else raise exception.Create('Unknown algorithm');
  end;
end;

{ TDitheringTask }

constructor TDitheringTask.Create(bmp: TBGRACustomBitmap;
  APalette: TBGRACustomApproxPalette; AInPlace: boolean; AIgnoreAlpha: boolean;
  ABounds: TRect);
begin
  FPalette := APalette;
  FSource := bmp;
  FBounds := ABounds;
  FIgnoreAlpha:= AIgnoreAlpha;
  if AInPlace then Destination := FSource;
end;

constructor TDitheringTask.Create(bmp: TBGRACustomBitmap;
  APalette: TBGRACustomApproxPalette; AInPlace: boolean; AIgnoreAlpha: boolean);
begin
  FPalette := APalette;
  FSource := bmp;
  FBounds := rect(0,0,bmp.Width,bmp.Height);
  FIgnoreAlpha:= AIgnoreAlpha;
  if AInPlace then Destination := FSource;
end;

{ TFloydSteinbergDitheringTask }

procedure TFloydSteinbergDitheringTask.DoExecute;
const
  ErrorPrecisionShift = 4;
  MaxColorDiffForDiffusion = 4096;
type
  TAccPixel = record
    red,green,blue,alpha: NativeInt;
  end;
  TLine = array of TAccPixel;

  procedure AddError(var dest: TAccPixel; const src: TAccPixel; factor: NativeInt);
  const maxError = 65536 shl ErrorPrecisionShift;
    minError = -(65536 shl ErrorPrecisionShift);
  begin
    dest.alpha += src.alpha * factor;
    if dest.alpha > maxError then dest.alpha := maxError;
    if dest.alpha < minError then dest.alpha := minError;
    dest.red += src.red * factor;
    if dest.red > maxError then dest.red := maxError;
    if dest.red < minError then dest.red := minError;
    dest.green += src.green * factor;
    if dest.green > maxError then dest.green := maxError;
    if dest.green < minError then dest.green := minError;
    dest.blue += src.blue * factor;
    if dest.blue > maxError then dest.blue := maxError;
    if dest.blue < minError then dest.blue := minError;
  end;

var
  w,h: NativeInt;

var
  p,pNext,pDest: PBGRAPixel;
  orig,cur,approxExp: TExpandedPixel;
  approx: TBGRAPixel;
  curPix,diff: TAccPixel;
  i: NativeInt;
  yWrite: NativeInt;
  tempLine, currentLine, nextLine: TLine;

  function ClampWordDiv(AValue: NativeInt): Word; inline;
  begin
    if AValue < 0 then AValue := -((-AValue) shr ErrorPrecisionShift) else AValue := AValue shr ErrorPrecisionShift;
    if AValue < 0 then
      result := 0
    else if AValue > 65535 then
      result := 65535
    else
      result := AValue;
  end;

  function Div16(AValue: NativeInt): NativeInt; inline;
  begin
    if AValue < 0 then
      result := -((-AValue) shr 4)
    else
      result := AValue shr 4;
  end;

begin
  w := FBounds.Right-FBounds.Left;
  h := FBounds.Bottom-FBounds.Top;
  if (w <= 0) or (h <= 0) then exit;
  setlength(currentLine,w);
  setlength(nextLine,w);
  for yWrite := 0 to h-1 do
  begin
    if GetShouldStop(yWrite) then break;
    p := FSource.ScanLine[yWrite+FBounds.Top]+FBounds.Left;
    pDest := FDestination.ScanLine[yWrite+FBounds.Top]+FBounds.Left;
    if yWrite < h-1 then
      pNext := FSource.ScanLine[yWrite+FBounds.Top+1]+FBounds.Left
    else
      pNext := nil;
    if odd(yWrite) then
    begin
      inc(p, w);
      inc(pDest, w);
      if pNext<>nil then inc(pNext, w);
      for i := w-1 downto 0 do
      begin
        dec(p);
        dec(pDest);
        if pNext<>nil then dec(pNext);
        if p^.alpha <> 0 then
        begin
          orig := GammaExpansion(p^);
          with currentLine[i] do
          begin
            curPix.alpha := alpha+NativeInt(orig.alpha shl ErrorPrecisionShift);
            curPix.red := red+NativeInt(orig.red shl ErrorPrecisionShift);
            curPix.green := green+NativeInt(orig.green shl ErrorPrecisionShift);
            curPix.blue := blue+NativeInt(orig.blue shl ErrorPrecisionShift);
            cur.alpha := ClampWordDiv(curPix.alpha);
            cur.red := ClampWordDiv(curPix.red);
            cur.green := ClampWordDiv(curPix.green);
            cur.blue := ClampWordDiv(curPix.blue);
          end;
          approx := FPalette.FindNearestColor(GammaCompression(cur), FIgnoreAlpha);
          approxExp := GammaExpansion(approx);
          diff.alpha := Div16(curPix.alpha - (approxExp.alpha shl ErrorPrecisionShift));
          if (approxExp.alpha = 0) or (cur.alpha = 0) then
          begin
            diff.red := 0;
            diff.green := 0;
            diff.blue := 0;
          end else
          begin
            diff.red := Div16(curPix.red - (approxExp.red shl ErrorPrecisionShift));
            diff.green := Div16(curPix.green - (approxExp.green shl ErrorPrecisionShift));
            diff.blue := Div16(curPix.blue - (approxExp.blue shl ErrorPrecisionShift));
          end;
          if i > 0 then
          begin
            if AbsRGBADiff(GammaExpansion((p-1)^),orig) < MaxColorDiffForDiffusion then
              AddError(currentLine[i-1], diff, 7);
          end;
          if nextLine <> nil then
          begin
            if i > 0 then
            begin
              if AbsRGBADiff(GammaExpansion((pNext-1)^),orig) < MaxColorDiffForDiffusion then
                AddError(nextLine[i-1], diff, 1);
            end;
            if AbsRGBADiff(GammaExpansion(pNext^),orig) < MaxColorDiffForDiffusion then
              AddError(nextLine[i], diff, 5);
            if i < w-1 then
            begin
              if AbsRGBADiff(GammaExpansion((pNext+1)^),orig) < MaxColorDiffForDiffusion then
                AddError(nextLine[i+1], diff, 3);
            end;
          end;
          pDest^ := approx;
        end;
      end
    end
    else
    for i := 0 to w-1 do
    begin
      if p^.alpha <> 0 then
      begin
        orig := GammaExpansion(p^);
        with currentLine[i] do
        begin
          curPix.alpha := alpha+NativeInt(orig.alpha shl ErrorPrecisionShift);
          curPix.red := red+NativeInt(orig.red shl ErrorPrecisionShift);
          curPix.green := green+NativeInt(orig.green shl ErrorPrecisionShift);
          curPix.blue := blue+NativeInt(orig.blue shl ErrorPrecisionShift);
          cur.alpha := ClampWordDiv(curPix.alpha);
          cur.red := ClampWordDiv(curPix.red);
          cur.green := ClampWordDiv(curPix.green);
          cur.blue := ClampWordDiv(curPix.blue);
        end;
        approx := FPalette.FindNearestColor(GammaCompression(cur), FIgnoreAlpha);
        approxExp := GammaExpansion(approx);
        diff.alpha := Div16(curPix.alpha - (approxExp.alpha shl ErrorPrecisionShift));
        if (approxExp.alpha = 0) or (cur.alpha = 0) then
        begin
          diff.red := 0;
          diff.green := 0;
          diff.blue := 0;
        end else
        begin
          diff.red := Div16(curPix.red - (approxExp.red shl ErrorPrecisionShift));
          diff.green := Div16(curPix.green - (approxExp.green shl ErrorPrecisionShift));
          diff.blue := Div16(curPix.blue - (approxExp.blue shl ErrorPrecisionShift));
        end;
        if i < w-1 then
        begin
          if AbsRGBADiff(GammaExpansion((p+1)^),orig) < MaxColorDiffForDiffusion then
            AddError(currentLine[i+1], diff, 7);
        end;
        if nextLine <> nil then
        begin
          if i > 0 then
          begin
            if AbsRGBADiff(GammaExpansion((pNext-1)^),orig) < MaxColorDiffForDiffusion then
              AddError(nextLine[i-1], diff, 3);
          end;
          if AbsRGBADiff(GammaExpansion(pNext^),orig) < MaxColorDiffForDiffusion then
            AddError(nextLine[i], diff, 5);
          if i < w-1 then
          begin
            if AbsRGBADiff(GammaExpansion((pNext+1)^),orig) < MaxColorDiffForDiffusion then
              AddError(nextLine[i+1], diff, 1);
          end;
        end;
        pDest^ := approx;
      end;
      inc(p);
      inc(pDest);
      if pNext<>nil then inc(pNext);
    end;
    tempLine := currentLine;
    currentLine := nextLine;
    nextLine := tempLine;
    if yWrite = h-2 then
      nextLine := nil
    else
      for i := 0 to w-1 do
      begin
        nextLine[i].red := 0;
        nextLine[i].green := 0;
        nextLine[i].blue := 0;
        nextLine[i].alpha := 0;
      end;
  end;
  FDestination.InvalidateBitmap;
end;

{ TNearestColorTask }

procedure TNearestColorTask.DoExecute;
var yb,xb: integer;
  psrc,pdest: PBGRAPixel;
begin
  for yb := FBounds.Top to FBounds.Bottom - 1 do
  begin
    if GetShouldStop(yb) then break;
    psrc := FSource.ScanLine[yb] + FBounds.Left;
    pdest := FDestination.ScanLine[yb] + FBounds.Left;
    for xb := FBounds.Right - FBounds.Left -1 downto 0 do
    begin
      pdest^ := FPalette.FindNearestColor(psrc^, FIgnoreAlpha);
      inc(pdest);
      inc(psrc);
    end;
  end;
  FDestination.InvalidateBitmap;
end;

end.

