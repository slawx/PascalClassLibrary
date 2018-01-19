{$IFDEF INTERFACE}

// TGBitmap<TGBitmapIndexX, TGBitmapIndexY, TGBitmapItem>
{$DEFINE TGMatrixIndexX := TGBitmapIndexX}
{$DEFINE TGMatrixIndexY := TGBitmapIndexY}
{$DEFINE TGMatrixItem := TGBitmapItem}
{$DEFINE TGMatrixIndex := TGBitmapIndex}
{$DEFINE TGMatrixRow := TGBitmapRow}
{$DEFINE TGMatrix := TGBitmapMatrix}
{$DEFINE INTERFACE}
{$I 'GenericMatrix.inc'}

  // TGBitmap<TGBitmapIndex, TGBitmapIndex, TGBitmapItem> = class
  TGBitmap = class(TGMatrix)
  private
    function GetWidth: TGBitmapIndexX;
    function GetHeight: TGBitmapIndexX;
    procedure SetWidth(Value: TGBitmapIndexX);
    procedure SetHeight(Value: TGBitmapIndexY);
  public
    property Pixels[X: TGBitmapIndexX; Y: TGBitmapIndexY]: TGBitmapItem
      read GetItemXY write PutItemXY;
    property Width: TGBitmapIndexX read GetWidth write SetWidth;
    property Height: TGBitmapIndexY read GetHeight write SetHeight;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION_USES}

  {$DEFINE IMPLEMENTATION_USES}
  {$I 'GenericMatrix.inc'}

{$UNDEF IMPLEMENTATION_USES}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

// TGBitmap<TGBitmapIndexX, TGBitmapIndexY, TGBitmapItem>
{$DEFINE TGMatrixIndexX := TGBitmapIndexX}
{$DEFINE TGMatrixIndexY := TGBitmapIndexY}
{$DEFINE TGMatrixItem := TGBitmapItem}
{$DEFINE TGMatrixIndex := TGBitmapIndex}
{$DEFINE TGMatrixRow := TGBitmapRow}
{$DEFINE TGMatrix := TGBitmapMatrix}
{$DEFINE IMPLEMENTATION}
{$I 'GenericMatrix.inc'}

function TGBitmap.GetWidth: TGBitmapIndexX;
begin
  Result := Count.X;
end;

function TGBitmap.GetHeight: TGBitmapIndexX;
begin
  Result := Count.Y;
end;

procedure TGBitmap.SetWidth(Value: TGBitmapIndexX);
begin
  Count := CreateIndex(Value, Count.Y);
end;

procedure TGBitmap.SetHeight(Value: TGBitmapIndexY);
begin
  Count := CreateIndex(Count.X, Value);
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}

