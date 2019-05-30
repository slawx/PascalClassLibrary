unit UDpiControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, FormEditingIntf, ProjectIntf,
  Controls, StdCtrls, fgl, Graphics, ComCtrls, ExtCtrls, LCLType, GraphType,
  Types;

type
   { TDpiFormFileDesc }

  TDpiFormFileDesc = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TDpiFont }

  TDpiFont = class
  private
    FOnChange: TNotifyEvent;
    FSize: Integer;
    function GetColor: TColor;
    function GetName: string;
    function GetPixelsPerInch: Integer;
    function GetStyle: TFontStyles;
    procedure SetColor(AValue: TColor);
    procedure SetName(AValue: string);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetPixelsPerInch(AValue: Integer);
    procedure SetSize(AValue: Integer);
    procedure DoChange;
    procedure SetStyle(AValue: TFontStyles);
  protected
    procedure ScreenChanged;
    function GetVclFont: TFont; virtual;
  public
    VclFont: TFont;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TDpiFont);
  published
    property Color: TColor read GetColor write SetColor;
    property Name: string read GetName write SetName;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Size: Integer read FSize write SetSize;
    property PixelsPerInch: Integer read GetPixelsPerInch write SetPixelsPerInch;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TDpiWinControl = class;

  { TDpiControl }

  TDpiControl = class(TComponent)
  private
    FFont: TDpiFont;
    FHeight: Integer;
    FLeft: Integer;
    FOnChangeBounds: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FTop: Integer;
    FWidth: Integer;
    FParent: TDpiWinControl;
    function GetAlign: TAlign;
    function GetBoundsRect: TRect;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    function GetEnabled: Boolean;
    function GetHint: string;
    function GetOnClick: TNotifyEvent;
    function GetShowHint: Boolean;
    function GetVisible: Boolean;
    procedure SetAlign(AValue: TAlign);
    procedure SetBoundsRect(AValue: TRect);
    procedure SetClientHeight(AValue: Integer);
    procedure SetClientWidth(AValue: Integer);
    procedure SetEnabled(AValue: Boolean);
    procedure SetFont(AValue: TDpiFont);
    procedure SetHint(AValue: string);
    procedure SetOnChangeBounds(AValue: TNotifyEvent);
    procedure SetOnClick(AValue: TNotifyEvent);
    procedure SetOnResize(AValue: TNotifyEvent);
    procedure SetShowHint(AValue: Boolean);
    procedure VclFormResize(Sender: TObject);
    procedure VclChangeBounds(Sender: TObject);
    procedure DoFormResize;
    procedure DoChangeBounds;
  protected
    procedure UpdateBounds; virtual;
    procedure FontChanged(Sender: TObject); virtual;
    function GetCaption: string; virtual;
    procedure SetParent(AValue: TDpiWinControl); virtual;
    procedure SetCaption(AValue: string); virtual;
    procedure SetHeight(AValue: Integer); virtual;
    procedure SetLeft(AValue: Integer); virtual;
    procedure SetTop(AValue: Integer); virtual;
    procedure SetVisible(AValue: Boolean); virtual;
    procedure SetWidth(AValue: Integer); virtual;
    function GetVclControl: TControl; virtual;
    procedure UpdateVclControl; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
  public
    procedure ScreenChanged; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure Show;
    procedure Hide;
    procedure Invalidate;
    procedure Repaint;
    procedure Update;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Parent: TDpiWinControl read FParent write SetParent;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property ClientWidth: Integer read GetClientWidth write SetClientWidth;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight;
  published
    property Hint: string read GetHint write SetHint;
    property Top: Integer read FTop write SetTop;
    property Left: Integer read FLeft write SetLeft;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Visible: Boolean read GetVisible write SetVisible;
    property Caption: string read GetCaption write SetCaption;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property ShowHint: Boolean read GetShowHint write SetShowHint;
    property Font: TDpiFont read FFont write SetFont;
    property Align: TAlign read GetAlign write SetAlign;
    property OnResize: TNotifyEvent read FOnResize write SetOnResize;
    property OnChangeBounds: TNotifyEvent read FOnChangeBounds write SetOnChangeBounds;
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
  end;

  TDpiControls = specialize TFPGObjectList<TDpiControl>;

  { TDpiGraphic }

  TDpiGraphic = class(TPersistent)
  protected
    function GetVclGraphic: TGraphic; virtual;
  public
    procedure LoadFromFile(const Filename: string);
  end;

  { TDpiRasterImage }

  TDpiRasterImage = class(TDpiGraphic)
  private
    function GetRawImage: TRawImage;
  protected
    function GetVclGraphic: TGraphic; override;
    function GetVclRasterImage: TRasterImage; virtual;
  public
    property RawImage: TRawImage read GetRawImage;
  end;

  { TDpiCanvas }

  TDpiCanvas = class
  private
    FFont: TDpiFont;
    function GetBrush: TBrush;
    function GetHandle: HDC;
    function GetHeight: Integer;
    function GetPen: TPen;
    function GetPixel(X, Y: Integer): TColor;
    function GetWidth: Integer;
    procedure SetBrush(AValue: TBrush);
    procedure SetFont(AValue: TDpiFont);
    procedure SetHandle(AValue: HDC);
    procedure SetPen(AValue: TPen);
    procedure SetPixel(X, Y: Integer; AValue: TColor);
  protected
    function GetVclCanvas: TCanvas; virtual;
  public
    VclCanvas: TCanvas;
    procedure FrameRect(Rect: TRect);
    procedure Rectangle(X1, Y1, X2, Y2: Integer);
    function TextWidth(Text: string): Integer;
    function TextHeight(Text: string): Integer;
    function TextExtent(Text: string): TSize;
    procedure TextOut(X, Y: Integer; Text: string);
    procedure TextRect(ARect: TRect; X, Y: Integer; Text: string);
    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);
    procedure FillRect(ARect: TRect);
    procedure FillRect(X1, Y1, X2, Y2: Integer);
    procedure Draw(X, Y: Integer; Source: TDpiGraphic);
    procedure CopyRect(Dest: TRect; SrcCanvas: TDpiCanvas; Source: TRect);
    constructor Create;
    destructor Destroy; override;
    property Handle: HDC read GetHandle write SetHandle;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  published
    property Brush: TBrush read GetBrush write SetBrush;
    property Pen: TPen read GetPen write SetPen;
    property Font: TDpiFont read FFont write SetFont;
  end;

  { TDpiGraphicControl }

  TDpiGraphicControl = class(TDpiControl)
  private
    FCanvas: TDpiCanvas;
    function GetOnPaint: TNotifyEvent;
    procedure SetCanvas(AValue: TDpiCanvas);
    procedure SetOnPaint(AValue: TNotifyEvent);
  protected
    procedure Paint; virtual;
    function GetVclControl: TControl; override;
    function GetVclGraphicControl: TGraphicControl; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Canvas: TDpiCanvas read FCanvas write SetCanvas;
    property OnPaint: TNotifyEvent read GetOnPaint write SetOnPaint;
  end;

  { TDpiWinControl }

  TDpiWinControl = class(TDpiControl)
  private
    function GetHandle: HWND;
    function GetOnKeyDown: TKeyEvent;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnKeyUp: TKeyEvent;
    procedure SetHandle(AValue: HWND);
    procedure SetOnKeyDown(AValue: TKeyEvent);
    procedure SetOnKeyPress(AValue: TKeyPressEvent);
    procedure SetOnKeyUp(AValue: TKeyEvent);
  protected
    function GetVclControl: TControl; override;
    function GetVclWinControl: TWinControl; virtual;
  public
    Controls: TDpiControls;
    procedure ScreenChanged; override;
    function ControlCount: Integer;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Handle: HWND read GetHandle write SetHandle;
  published
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyUp: TKeyEvent read GetOnKeyUp write SetOnKeyUp;
  end;

  { TDpiCustomControl }

  TDpiCustomControl = class(TDpiWinControl)
  private
    function GetOnPaint: TNotifyEvent;
    procedure SetOnPaint(AValue: TNotifyEvent);
  protected
    function GetVclWinControl: TWinControl; override;
    function GetVclCustomControl: TCustomControl; virtual;
  published
    property OnPaint: TNotifyEvent read GetOnPaint write SetOnPaint;
  end;

  { TDpiForm }

  TDpiForm = class(TDpiCustomControl)
  private
    function GetBorderIcons: TBorderIcons;
    function GetBorderStyle: TBorderStyle;
    function GetCanvas: TDpiCanvas;
    function GetFormState: TFormState;
    function GetModalResult: TModalResult;
    function GetOnClose: TCloseEvent;
    function GetOnCreate: TNotifyEvent;
    function GetOnDeactivate: TNotifyEvent;
    function GetOnDestroy: TNotifyEvent;
    function GetOnHide: TNotifyEvent;
    function GetOnShow: TNotifyEvent;
    procedure SetBorderIcons(AValue: TBorderIcons);
    procedure SetBorderStyle(AValue: TBorderStyle);
    procedure SetModalResult(AValue: TModalResult);
    procedure SetOnClose(AValue: TCloseEvent);
    procedure SetOnCreate(AValue: TNotifyEvent);
    procedure SetOnDeactivate(AValue: TNotifyEvent);
    procedure SetOnDestroy(AValue: TNotifyEvent);
    procedure SetOnHide(AValue: TNotifyEvent);
    procedure SetOnShow(AValue: TNotifyEvent);
    procedure DoOnCreate;
  protected
    procedure CreateParams(var p: TCreateParams); virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetVclCustomControl: TCustomControl; override;
    function GetVclForm: TForm; virtual;
  public
    VclForm: TForm;
    property Canvas: TDpiCanvas read GetCanvas;
    property ModalResult: TModalResult read GetModalResult write SetModalResult;
    function ShowModal: Integer; virtual;
    procedure Close;
    procedure BringToFront;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FormState: TFormState read GetFormState;
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle;
    property BorderIcons: TBorderIcons read GetBorderIcons write SetBorderIcons;
    property OnShow: TNotifyEvent read GetOnShow write SetOnShow;
    property OnHide: TNotifyEvent read GetOnHide write SetOnHide;
    property OnCreate: TNotifyEvent read GetOnCreate write SetOnCreate;
    property OnDestroy: TNotifyEvent read GetOnDestroy write SetOnDestroy;
    property OnDeactivate: TNotifyEvent read GetOnDeactivate write SetOnDeactivate;
    property OnClose: TCloseEvent read GetOnClose write SetOnClose;
  end;

  TDpiForms = specialize TFPGObjectList<TDpiForm>;

  { TDpiButton }

  TDpiButton = class(TDpiControl)
  private
  protected
    function GetVclControl: TControl; override;
  public
    VclButton: TButton;
    destructor Destroy; override;
  published
  end;

  { TDpiListBox }

  TDpiListBox = class(TDpiControl)
  private
  protected
    function GetVclControl: TControl; override;
  public
    VclListBox: TListBox;
    destructor Destroy; override;
  end;

  { TDpiScrollBar }

  TDpiScrollBar = class(TDpiControl)
  private
    function GetBorderSpacing: TControlBorderSpacing;
    function GetKind: TScrollBarKind;
    function GetMax: Integer;
    function GetMin: Integer;
    function GetOnChange: TNotifyEvent;
    function GetPageSize: Integer;
    function GetPosition: Integer;
    procedure SetBorderSpacing(AValue: TControlBorderSpacing);
    procedure SetKind(AValue: TScrollBarKind);
    procedure SetMax(AValue: Integer);
    procedure SetMin(AValue: Integer);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetPageSize(AValue: Integer);
    procedure SetPosition(AValue: Integer);
  protected
    function GetVclControl: TControl; override;
  public
    VclScrollBar: TScrollBar;
    destructor Destroy; override;
  published
    property PageSize: Integer read GetPageSize write SetPageSize;
    property Min: Integer read GetMin write SetMin;
    property Max: Integer read GetMax write SetMax;
    property Position: Integer read GetPosition write SetPosition;
    property BorderSpacing: TControlBorderSpacing read GetBorderSpacing write SetBorderSpacing;
    property Kind: TScrollBarKind read GetKind write SetKind;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
  end;

  { TDpiBitmap }

  TDpiBitmap = class(TDpiRasterImage)
  private
    FCanvas: TDpiCanvas;
    function GetCanvas: TDpiCanvas;
    function GetHeight: Integer;
    function GetPixelFormat: TPixelFormat;
    function GetScanLine(Row: Integer): Pointer;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetPixelFormat(AValue: TPixelFormat);
    procedure SetWidth(AValue: Integer);
  protected
    function GetVclBitmap: TCustomBitmap; virtual;
    function GetVclRasterImage: TRasterImage; override;
  public
    VclBitmap: TBitmap;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetSize(Width, Height: Integer);
    constructor Create;
    destructor Destroy; override;
    property ScanLine[Row: Integer]: Pointer read GetScanLine;
  published
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
    property Canvas: TDpiCanvas read GetCanvas;
  end;

  { TDpiPicture }

  TDpiPicture = class(TPersistent)
  private
    FBitmap: TDpiBitmap;
    procedure SetBitmap(AValue: TDpiBitmap);
  published
    procedure LoadFromFile(FileName: string);
    property Bitmpa: TDpiBitmap read FBitmap write SetBitmap;
  end;

  { TDpiImage }

  TDpiImage = class(TDpiControl)
  private
    FDpiPicture: TDpiPicture;
    FStretch: Boolean;
    procedure SetPicture(AValue: TDpiPicture);
    procedure SetStretch(AValue: Boolean);
  protected
  public
    VclImage: TImage;
    function GetVclControl: TControl; override;
    destructor Destroy; override;
  published
    property Stretch: Boolean read FStretch write SetStretch;
    property Picture: TDpiPicture read FDpiPicture write SetPicture;
  end;

  { TDpiPaintBox }

  TDpiPaintBox = class(TDpiGraphicControl)
  private
    function GetOnPaint: TNotifyEvent;
    procedure SetOnPaint(AValue: TNotifyEvent);
  public
    VclPaintBox: TPaintBox;
    function GetVclControl: TControl; override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  { TDpiScreen }

  TDpiScreen = class
  private
    FDpi: Integer;
    FActiveForm: TDpiForm;
    function GetActiveForm: TDpiForm;
    function GetFormCount: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetDpi(AValue: Integer);
    procedure UpdateForms;
  public
    Forms: TDpiForms;
    constructor Create;
    destructor Destroy; override;
    property FormCount: Integer read GetFormCount;
    property ActiveForm: TDpiForm read GetActiveForm;
  published
    property Dpi: Integer read FDpi write SetDpi;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  { TDpiJpegImage }

  TDpiJpegImage = class(TDpiBitmap)
  protected
    function GetVclBitmap: TCustomBitmap; override;
    function GetVclJpeg: TJPEGImage; virtual;
  public
    VclJpeg: TJPEGImage;
  end;

  { TDpiPortableNetworkGraphic }

  TDpiPortableNetworkGraphic = class(TDpiBitmap)
  protected
    function GetVclBitmap: TCustomBitmap; override;
    function GetVclPng: TPortableNetworkGraphic; virtual;
  public
    VclPng: TPortableNetworkGraphic;
  end;

var
  DpiFormFileDesc: TDpiFormFileDesc;
  DpiScreen: TDpiScreen;

procedure Register;


implementation

resourcestring
  SDpiFormTitle = 'DpiForm form';
  SDpiFormDescription = 'DPI aware form';

procedure Register;
begin
  FormEditingHook.RegisterDesignerBaseClass(TDpiForm);
  DpiFormFileDesc := TDpiFormFileDesc.Create;
  RegisterProjectFileDescriptor(DpiFormFileDesc);
  RegisterComponents('DpiControls', [TDpiButton, TDpiImage, TDpiPaintBox, TDpiListBox]);
end;

function ScaleToVcl(Value: Integer): Integer;
begin
  Result := Round(Value * DpiScreen.Dpi / 96);
end;

function ScaleFromVcl(Value: Integer): Integer;
begin
  Result := Round(Value * 96 / DpiScreen.Dpi);
end;

function ScaleRectToVcl(Value: TRect): TRect;
begin
  Result.Left := ScaleToVcl(Value.Left);
  Result.Top := ScaleToVcl(Value.Top);
  Result.Right := ScaleToVcl(Value.Right);
  Result.Bottom := ScaleToVcl(Value.Bottom);
end;

function ScaleRectFromVcl(Value: TRect): TRect;
begin
  Result.Left := ScaleFromVcl(Value.Left);
  Result.Top := ScaleFromVcl(Value.Top);
  Result.Right := ScaleFromVcl(Value.Right);
  Result.Bottom := ScaleFromVcl(Value.Bottom);
end;

{ TDpiJpegImage }

function TDpiJpegImage.GetVclBitmap: TCustomBitmap;
begin
  Result := GetVclJpeg;
end;

function TDpiJpegImage.GetVclJpeg: TJPEGImage;
begin
  if not Assigned(VclJpeg) then VclJpeg := TJPEGImage.Create;
  Result := VclJpeg;
end;

{ TDpiPortableNetworkGraphic }

function TDpiPortableNetworkGraphic.GetVclBitmap: TCustomBitmap;
begin
  Result := GetVclPng;
end;

function TDpiPortableNetworkGraphic.GetVclPng: TPortableNetworkGraphic;
begin
  if not Assigned(VclPng) then VclPng := TPortableNetworkGraphic.Create;
  Result := VclPng;
end;

{ TDpiCustomControl }

function TDpiCustomControl.GetOnPaint: TNotifyEvent;
begin
  Result := GetVclCustomControl.OnPaint;
end;

procedure TDpiCustomControl.SetOnPaint(AValue: TNotifyEvent);
begin
  GetVclCustomControl.OnPaint := AValue;
end;

function TDpiCustomControl.GetVclWinControl: TWinControl;
begin
  Result := GetVclCustomControl;
end;

function TDpiCustomControl.GetVclCustomControl: TCustomControl;
begin
  Result := nil;
end;

{ TDpiScrollBar }

function TDpiScrollBar.GetBorderSpacing: TControlBorderSpacing;
begin
  Result := VclScrollBar.BorderSpacing;
end;

function TDpiScrollBar.GetKind: TScrollBarKind;
begin
  Result := VclScrollBar.Kind;
end;

function TDpiScrollBar.GetMax: Integer;
begin
  Result := VclScrollBar.Max;
end;

function TDpiScrollBar.GetMin: Integer;
begin
  Result := VclScrollBar.Min;
end;

function TDpiScrollBar.GetOnChange: TNotifyEvent;
begin
  Result := VclScrollBar.OnChange;
end;

function TDpiScrollBar.GetPageSize: Integer;
begin
  Result := VclScrollBar.PageSize;
end;

function TDpiScrollBar.GetPosition: Integer;
begin
  Result := VclScrollBar.Position;
end;

procedure TDpiScrollBar.SetBorderSpacing(AValue: TControlBorderSpacing);
begin
  VclScrollBar.BorderSpacing := AValue;
end;

procedure TDpiScrollBar.SetKind(AValue: TScrollBarKind);
begin
  VclScrollBar.Kind := AValue;
end;

procedure TDpiScrollBar.SetMax(AValue: Integer);
begin
  VclScrollBar.Max := AValue;
end;

procedure TDpiScrollBar.SetMin(AValue: Integer);
begin
  VclScrollBar.Min := Avalue;
end;

procedure TDpiScrollBar.SetOnChange(AValue: TNotifyEvent);
begin
  VclScrollBar.OnChange := AValue;
end;

procedure TDpiScrollBar.SetPageSize(AValue: Integer);
begin
  VclScrollBar.PageSize := AValue;
end;

procedure TDpiScrollBar.SetPosition(AValue: Integer);
begin
  VclScrollBar.Position := AValue;
end;

function TDpiScrollBar.GetVclControl: TControl;
begin
  if not Assigned(VclScrollBar) then VclScrollBar := TScrollBar.Create(nil);
  Result := VclScrollBar;
end;

destructor TDpiScrollBar.Destroy;
begin
  FreeAndNil(VclScrollBar);
  inherited Destroy;
end;

{ TDpiRasterImage }

function TDpiRasterImage.GetRawImage: TRawImage;
begin
  Result := GetVclRasterImage.RawImage;
end;

function TDpiRasterImage.GetVclRasterImage: TRasterImage;
begin
  Result := GetVclRasterImage;
end;

function TDpiRasterImage.GetVclGraphic: TGraphic;
begin
  Result := GetVclRasterImage;
end;

{ TDpiGraphic }

function TDpiGraphic.GetVclGraphic: TGraphic;
begin
  Result := nil;
end;

procedure TDpiGraphic.LoadFromFile(const Filename: string);
begin
  GetVclGraphic.LoadFromFile(FileName);
end;

{ TDpiBitmap }

function TDpiBitmap.GetHeight: Integer;
begin
  Result := ScaleFromVcl(GetVclBitmap.Height);
end;

function TDpiBitmap.GetCanvas: TDpiCanvas;
begin
  if not Assigned(FCanvas) then FCanvas := TDpiCanvas.Create;
  Result := FCanvas;
end;

function TDpiBitmap.GetPixelFormat: TPixelFormat;
begin
  Result := GetVclBitmap.PixelFormat;
end;

function TDpiBitmap.GetScanLine(Row: Integer): Pointer;
begin
  Result := GetVclBitmap.ScanLine[Row];
end;

function TDpiBitmap.GetWidth: Integer;
begin
  Result := ScaleFromVcl(GetVclBitmap.Width);
end;

procedure TDpiBitmap.SetHeight(AValue: Integer);
begin
  GetVclBitmap.Height := ScaleToVcl(AValue);
end;

procedure TDpiBitmap.SetPixelFormat(AValue: TPixelFormat);
begin
  GetVclBitmap.PixelFormat := AValue;
end;

procedure TDpiBitmap.SetWidth(AValue: Integer);
begin
  GetVclBitmap.Width := ScaleToVcl(AValue);
end;

function TDpiBitmap.GetVclBitmap: TCustomBitmap;
begin
  if not Assigned(VclBitmap) then begin
    VclBitmap := TBitmap.Create;
    Canvas.VclCanvas := VclBitmap.Canvas;
  end;
  Result := VclBitmap;
end;

procedure TDpiBitmap.BeginUpdate;
begin
  GetVclBitmap.BeginUpdate;
end;

procedure TDpiBitmap.EndUpdate;
begin
  GetVclBitmap.EndUpdate;
end;

procedure TDpiBitmap.SetSize(Width, Height: Integer);
begin
  GetVclBitmap.SetSize(ScaleToVcl(Width), ScaleToVcl(Height));
end;

constructor TDpiBitmap.Create;
begin
end;

destructor TDpiBitmap.Destroy;
begin
  FreeAndNil(FCanvas);
  FreeAndNil(VclBitmap);
  inherited;
end;

function TDpiBitmap.GetVclRasterImage: TRasterImage;
begin
  Result := GetVclBitmap;
end;

{ TDpiListBox }

function TDpiListBox.GetVclControl: TControl;
begin
  if not Assigned(VclListBox) then VclListBox := TListBox.Create(nil);
  Result := VclListBox;
end;

destructor TDpiListBox.Destroy;
begin
  FreeAndNil(VclListBox);
  inherited Destroy;
end;

{ TDpiPaintBox }

function TDpiPaintBox.GetOnPaint: TNotifyEvent;
begin
  Result := VclPaintBox.OnPaint;
end;

procedure TDpiPaintBox.SetOnPaint(AValue: TNotifyEvent);
begin
  VclPaintBox.OnPaint := AValue;
end;

function TDpiPaintBox.GetVclControl: TControl;
begin
  if not Assigned(VclPaintBox) then VclPaintBox := TPaintBox.Create(nil);
  Result := VclPaintBox;
end;

constructor TDpiPaintBox.Create(TheOwner: TComponent);
begin
  inherited;
  Canvas := TDpiCanvas.Create;
  Canvas.VclCanvas := VclPaintBox.Canvas;
  Canvas.Font.VclFont := VclPaintBox.Canvas.Font;
  UpdateVclControl;
  ScreenChanged;
end;

destructor TDpiPaintBox.Destroy;
begin
  FreeAndNil(VclPaintBox);
  inherited;
end;

{ TDpiPicture }

procedure TDpiPicture.SetBitmap(AValue: TDpiBitmap);
begin
  if FBitmap = AValue then Exit;
  FBitmap := AValue;
end;

procedure TDpiPicture.LoadFromFile(FileName: string);
begin
end;


{ TDpiCanvas }

function TDpiCanvas.GetBrush: TBrush;
begin
  Result := GetVclCanvas.Brush;
end;

function TDpiCanvas.GetHandle: HDC;
begin
  Result := GetVclCanvas.Handle;
end;

function TDpiCanvas.GetHeight: Integer;
begin
  Result := ScaleFromVcl(GetVclCanvas.Height);
end;

function TDpiCanvas.GetPen: TPen;
begin
  Result := GetVclCanvas.Pen;
end;

function TDpiCanvas.GetPixel(X, Y: Integer): TColor;
begin
  Result := GetVclCanvas.Pixels[ScaleToVcl(X), ScaleToVcl(Y)];
end;

function TDpiCanvas.GetWidth: Integer;
begin
  Result := ScaleFromVcl(GetVclCanvas.Width);
end;

procedure TDpiCanvas.SetBrush(AValue: TBrush);
begin
  GetVclCanvas.Brush := AValue;
end;

procedure TDpiCanvas.SetFont(AValue: TDpiFont);
begin
  if FFont = AValue then Exit;
  FFont := AValue;
end;

procedure TDpiCanvas.SetHandle(AValue: HDC);
begin
  GetVclCanvas.Handle := AValue;
end;

procedure TDpiCanvas.SetPen(AValue: TPen);
begin
  GetVclCanvas.Pen := AValue;
end;

procedure TDpiCanvas.SetPixel(X, Y: Integer; AValue: TColor);
begin
  GetVclCanvas.Pixels[ScaleToVcl(X), ScaleToVcl(Y)] := AValue;
end;

function TDpiCanvas.GetVclCanvas: TCanvas;
begin
  if not Assigned(VclCanvas) then VclCanvas := TCanvas.Create;
  Result := VclCanvas;
end;

procedure TDpiCanvas.FrameRect(Rect: TRect);
begin
  GetVclCanvas.FrameRect(ScaleRectToVcl(Rect));
end;

procedure TDpiCanvas.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  GetVclCanvas.Rectangle(ScaleToVcl(X1), ScaleToVcl(Y1), ScaleToVcl(X2), ScaleToVcl(Y2));
end;

function TDpiCanvas.TextWidth(Text: string): Integer;
begin
  Result := ScaleFromVcl(GetVclCanvas.TextWidth(Text));
end;

function TDpiCanvas.TextHeight(Text: string): Integer;
begin
  Result := ScaleFromVcl(GetVclCanvas.TextHeight(Text));
end;

function TDpiCanvas.TextExtent(Text: string): TSize;
begin
  Result := GetVclCanvas.TextExtent(Text);
end;

procedure TDpiCanvas.TextOut(X, Y: Integer; Text: string);
begin
  GetVclCanvas.TextOut(ScaleToVcl(X), ScaleToVcl(Y), Text);
end;

procedure TDpiCanvas.TextRect(ARect: TRect; X, Y: Integer; Text: string);
begin
  GetVclCanvas.TextRect(ARect, X, Y, Text);
end;

procedure TDpiCanvas.MoveTo(X, Y: Integer);
begin
  GetVclCanvas.MoveTo(ScaleToVcl(X), ScaleToVcl(Y));
end;

procedure TDpiCanvas.LineTo(X, Y: Integer);
begin
  GetVclCanvas.LineTo(ScaleToVcl(X), ScaleToVcl(Y));
end;

procedure TDpiCanvas.FillRect(ARect: TRect);
begin
  GetVclCanvas.FillRect(ScaleRectToVcl(ARect));
end;

procedure TDpiCanvas.FillRect(X1, Y1, X2, Y2: Integer);
begin
  GetVclCanvas.FillRect(ScaleToVcl(X1), ScaleToVcl(Y1), ScaleToVcl(X2), ScaleToVcl(Y2));
end;

procedure TDpiCanvas.Draw(X, Y: Integer; Source: TDpiGraphic);
begin
  GetVclCanvas.Draw(ScaleToVcl(X), ScaleToVcl(Y), Source.GetVclGraphic);
end;

procedure TDpiCanvas.CopyRect(Dest: TRect; SrcCanvas: TDpiCanvas;
  Source: TRect);
begin
  GetVclCanvas.CopyRect(Dest, SrcCanvas.VclCanvas, Source);
end;

constructor TDpiCanvas.Create;
begin
  FFont := TDpiFont.Create;
end;

destructor TDpiCanvas.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

{ TDpiGraphicControl }

procedure TDpiGraphicControl.SetCanvas(AValue: TDpiCanvas);
begin
  if FCanvas = AValue then Exit;
  FCanvas := AValue;
end;

function TDpiGraphicControl.GetOnPaint: TNotifyEvent;
begin
  Result := nil;
end;

procedure TDpiGraphicControl.SetOnPaint(AValue: TNotifyEvent);
begin

end;

procedure TDpiGraphicControl.Paint;
begin
end;

function TDpiGraphicControl.GetVclControl: TControl;
begin
  Result := GetVclGraphicControl;
end;

function TDpiGraphicControl.GetVclGraphicControl: TGraphicControl;
begin
  Result := nil;
end;

constructor TDpiGraphicControl.Create(TheOwner: TComponent);
begin
  inherited;
  FCanvas := TDpiCanvas.Create;
end;

destructor TDpiGraphicControl.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited;
end;


{ TDpiImage }

procedure TDpiImage.SetStretch(AValue: Boolean);
begin
  if FStretch = AValue then Exit;
  FStretch := AValue;
  VclImage.Stretch := AValue;
end;

procedure TDpiImage.SetPicture(AValue: TDpiPicture);
begin
  if FDpiPicture = AValue then Exit;
  FDpiPicture := AValue;
end;

function TDpiImage.GetVclControl: TControl;
begin
  if not Assigned(VclImage) then VclImage := TImage.Create(nil);
  Result := VclImage;
end;

destructor TDpiImage.Destroy;
begin
  FreeAndNil(VclImage);
  inherited Destroy;
end;

{ TDpiFont }

procedure TDpiFont.SetSize(AValue: Integer);
begin
  if FSize = AValue then Exit;
  FSize := AValue;
  DoChange;
end;

procedure TDpiFont.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TDpiFont.SetStyle(AValue: TFontStyles);
begin
  GetVclFont.Style := AValue;
end;

procedure TDpiFont.ScreenChanged;
begin
  DoChange;
end;

function TDpiFont.GetVclFont: TFont;
begin
  if not Assigned(VclFont) then VclFont := TFont.Create;
  Result := VclFont;
end;

procedure TDpiFont.SetOnChange(AValue: TNotifyEvent);
begin
  if FOnChange = AValue then Exit;
  FOnChange := AValue;
end;

procedure TDpiFont.SetPixelsPerInch(AValue: Integer);
begin
  GetVclFont.PixelsPerInch := PixelsPerInch;
end;

function TDpiFont.GetName: string;
begin
  Result := GetVclFont.Name;
end;

function TDpiFont.GetColor: TColor;
begin
  Result := GetVclFont.Color;
end;

function TDpiFont.GetPixelsPerInch: Integer;
begin
  Result := GetVclFont.PixelsPerInch;
end;

function TDpiFont.GetStyle: TFontStyles;
begin
  Result := GetVclFont.Style;
end;

procedure TDpiFont.SetColor(AValue: TColor);
begin
  GetVclFont.Color := AValue;
end;

procedure TDpiFont.SetName(AValue: string);
begin
  GetVclFont.Name := AValue;
end;

constructor TDpiFont.Create;
begin
  Size := 8;
end;

destructor TDpiFont.Destroy;
begin
  inherited Destroy;
end;

procedure TDpiFont.Assign(Source: TDpiFont);
begin
  GetVclFont.Assign(Source.GetVclFont);
  Size := Source.Size;
  FOnChange := Source.FOnChange;
end;

{ TDpiWinControl }

function TDpiWinControl.GetHandle: HWND;
begin
  Result := GetVclWinControl.Handle;
end;

function TDpiWinControl.GetOnKeyDown: TKeyEvent;
begin
  Result := GetVclWinControl.OnKeyDown;
end;

function TDpiWinControl.GetOnKeyPress: TKeyPressEvent;
begin
  Result := GetVclWinControl.OnKeyPress;
end;

function TDpiWinControl.GetOnKeyUp: TKeyEvent;
begin
  Result := GetVclWinControl.OnKeyUp;
end;

procedure TDpiWinControl.SetHandle(AValue: HWND);
begin
  GetVclWinControl.Handle := AValue;
end;

procedure TDpiWinControl.SetOnKeyDown(AValue: TKeyEvent);
begin
  GetVclWinControl.OnKeyDown := AValue;
end;

procedure TDpiWinControl.SetOnKeyPress(AValue: TKeyPressEvent);
begin
  GetVclWinControl.OnKeyPress := AValue;
end;

procedure TDpiWinControl.SetOnKeyUp(AValue: TKeyEvent);
begin
  GetVclWinControl.OnKeyUp := AValue;

end;

function TDpiWinControl.GetVclControl: TControl;
begin
  Result := GetVclWinControl;
end;

function TDpiWinControl.GetVclWinControl: TWinControl;
begin
  Result := nil;
end;

procedure TDpiWinControl.ScreenChanged;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Controls.Count - 1 do
    Controls[I].ScreenChanged;
end;

function TDpiWinControl.ControlCount: Integer;
begin
  Result := Controls.Count;
end;

constructor TDpiWinControl.Create(TheOwner: TComponent);
begin
  Controls := TDpiControls.Create;
  Controls.FreeObjects := False;
  inherited;
end;

destructor TDpiWinControl.Destroy;
begin
  FreeAndNil(Controls);
  inherited Destroy;
end;

{ TDpiScreen }

procedure TDpiScreen.SetDpi(AValue: Integer);
begin
  if FDpi = AValue then Exit;
  FDpi := AValue;
  UpdateForms;
end;

function TDpiScreen.GetWidth: Integer;
begin
  Result := ScaleFromVcl(Screen.Width);
end;

function TDpiScreen.GetHeight: Integer;
begin
  Result := ScaleFromVcl(Screen.Height);
end;

function TDpiScreen.GetFormCount: Integer;
begin
  Result := Forms.Count;
end;

function TDpiScreen.GetActiveForm: TDpiForm;
begin
  Result := FActiveForm;
end;

procedure TDpiScreen.UpdateForms;
var
  I: Integer;
begin
  for I := 0 to Forms.Count - 1 do
    Forms[I].ScreenChanged;
end;

constructor TDpiScreen.Create;
begin
  Forms := TDpiForms.Create;
  Forms.FreeObjects := False;
  Dpi := 96;
end;

destructor TDpiScreen.Destroy;
begin
  FreeAndNil(Forms);
  inherited Destroy;
end;

{ TDpiWinControl }


{ TDpiButton }

function TDpiButton.GetVclControl: TControl;
begin
  if not Assigned(VclButton) then VclButton := TButton.Create(nil);
  Result := VclButton;
end;

destructor TDpiButton.Destroy;
begin
  FreeAndNil(VclButton);
  inherited;
end;

{ TDpiControl }

procedure TDpiControl.SetTop(AValue: Integer);
begin
  if FTop = AValue then Exit;
  FTop := AValue;
  UpdateBounds;
end;

procedure TDpiControl.SetVisible(AValue: Boolean);
begin
  GetVclControl.Visible := AValue;
end;

procedure TDpiControl.SetWidth(AValue: Integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
  UpdateBounds;
end;

function TDpiControl.GetVclControl: TControl;
begin
  Result := nil;
end;

procedure TDpiControl.UpdateVclControl;
begin
  GetVclControl.OnResize := @VclFormResize;
  GetVclControl.OnChangeBounds := @VclChangeBounds;
end;

procedure TDpiControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  // TODO
end;

procedure TDpiControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  // TODO
end;

procedure TDpiControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  // TODO
end;

procedure TDpiControl.ScreenChanged;
begin
  UpdateBounds;
  Font.ScreenChanged;
end;

procedure TDpiControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
  UpdateBounds;
end;

procedure TDpiControl.Show;
begin
  Visible := True;
end;

procedure TDpiControl.Hide;
begin
  Visible := False;
end;

procedure TDpiControl.Invalidate;
begin
  GetVclControl.Invalidate;
end;

procedure TDpiControl.Repaint;
begin
  GetVclControl.Repaint;
end;

procedure TDpiControl.Update;
begin
  GetVclControl.Update;
end;

constructor TDpiControl.Create(TheOwner: TComponent);
begin
  inherited;
  FFont := TDpiFont.Create;
  FFont.OnChange := @FontChanged;
  if Assigned(TheOwner) and (TheOwner is TDpiWinControl) then
    Parent := TDpiWinControl(TheOwner);
  GetVclControl;
  UpdateVclControl;
  ScreenChanged;
end;

destructor TDpiControl.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TDpiControl.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then Exit;
  FLeft := AValue;
  UpdateBounds;
end;

procedure TDpiControl.SetCaption(AValue: string);
begin
  GetVclControl.Caption := AValue;
end;

procedure TDpiControl.SetParent(AValue: TDpiWinControl);
begin
  if FParent = AValue then Exit;
  if Assigned(FParent) then begin
    FParent.Controls.Remove(Self);
    if Assigned(FParent) and (FParent is TDpiWinControl) then
      GetVclControl.Parent := nil;
  end;
  FParent := AValue;
  if Assigned(FParent) then begin
    FParent.Controls.Add(Self);
    if Assigned(FParent) and (FParent is TDpiWinControl) then
      GetVclControl.Parent := TDpiWinControl(FParent).GetVclWinControl;
  end;
end;

procedure TDpiControl.SetFont(AValue: TDpiFont);
begin
  if FFont = AValue then Exit;
  FFont := AValue;
end;

procedure TDpiControl.SetHint(AValue: string);
begin
  GetVclControl.Hint := AValue;
end;

function TDpiControl.GetBoundsRect: TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function TDpiControl.GetAlign: TAlign;
begin
  Result := GetVclControl.Align;
end;

function TDpiControl.GetClientHeight: Integer;
begin
  Result := ScaleFromVcl(GetVclControl.ClientHeight);
end;

function TDpiControl.GetClientWidth: Integer;
begin
  Result := ScaleFromVcl(GetVclControl.ClientWidth);
end;

function TDpiControl.GetEnabled: Boolean;
begin
  Result := GetVclControl.Enabled;
end;

function TDpiControl.GetHint: string;
begin
  Result := GetVclControl.Hint;
end;

function TDpiControl.GetOnClick: TNotifyEvent;
begin
  Result := GetVclControl.OnClick;
end;

function TDpiControl.GetShowHint: Boolean;
begin
  Result := GetVclControl.ShowHint;
end;

function TDpiControl.GetVisible: Boolean;
begin
  Result := GetVclControl.Visible;
end;

procedure TDpiControl.SetAlign(AValue: TAlign);
begin
  GetVclControl.Align := AValue;
end;

procedure TDpiControl.SetBoundsRect(AValue: TRect);
begin
  SetBounds(AValue.Left, AValue.Top, AValue.Right - AValue.Left, AValue.Bottom - AValue.Top);
end;

procedure TDpiControl.SetClientHeight(AValue: Integer);
begin
  GetVclControl.ClientHeight := ScaletoVcl(AValue);
end;

procedure TDpiControl.SetClientWidth(AValue: Integer);
begin
  GetVclControl.ClientWidth := ScaletoVcl(AValue);
end;

procedure TDpiControl.SetEnabled(AValue: Boolean);
begin
  GetVclControl.Enabled := AValue;
end;

procedure TDpiControl.SetOnChangeBounds(AValue: TNotifyEvent);
begin
  if FOnChangeBounds = AValue then Exit;
  FOnChangeBounds := AValue;
end;

procedure TDpiControl.SetOnClick(AValue: TNotifyEvent);
begin
  GetVclControl.OnClick := AValue;
end;

procedure TDpiControl.SetOnResize(AValue: TNotifyEvent);
begin
  if FOnResize = AValue then Exit;
  FOnResize := AValue;
end;

procedure TDpiControl.SetShowHint(AValue: Boolean);
begin
  GetVclControl.ShowHint := AValue;
end;

procedure TDpiControl.VclFormResize(Sender: TObject);
begin
  BoundsRect := ScaleRectFromVcl(GetVclControl.BoundsRect);
  DoFormResize;
end;

procedure TDpiControl.VclChangeBounds(Sender: TObject);
begin
  BoundsRect := ScaleRectFromVcl(GetVclControl.BoundsRect);
  DoChangeBounds;
end;

procedure TDpiControl.DoFormResize;
begin
  if Assigned(FOnResize) then FOnResize(Self);
end;

procedure TDpiControl.DoChangeBounds;
begin
  if Assigned(FOnChangeBounds) then FOnChangeBounds(Self);
end;

function TDpiControl.GetCaption: string;
begin
  Result := GetVclControl.Caption;
end;

procedure TDpiControl.FontChanged(Sender: TObject);
begin
  GetVclControl.Font.Size := ScaleToVcl(Font.Size);
end;

procedure TDpiControl.UpdateBounds;
begin
  GetVclControl.BoundsRect := ScaleRectToVcl(BoundsRect);
end;

procedure TDpiControl.SetHeight(AValue: Integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
  UpdateBounds;
end;

{ TDpiFormFileDesc }

constructor TDpiFormFileDesc.Create;
begin
  inherited Create;
  Name := 'DpiForm form';
  ResourceClass := TDpiForm;
  UseCreateFormStatements := False;
end;

function TDpiFormFileDesc.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection;
  Result := Result + ', UDpiForm';
end;

function TDpiFormFileDesc.GetLocalizedName: string;
begin
  Result := SDpiFormTitle;
end;

function TDpiFormFileDesc.GetLocalizedDescription: string;
begin
  Result := SDpiFormDescription;
end;

{ TDpiForm }

function TDpiForm.GetBorderIcons: TBorderIcons;
begin
  Result := VclForm.BorderIcons;
end;

function TDpiForm.GetBorderStyle: TBorderStyle;
begin
  Result := VclForm.BorderStyle;
end;

function TDpiForm.GetCanvas: TDpiCanvas;
begin
  Result := Canvas;
end;

function TDpiForm.GetFormState: TFormState;
begin
  Result := VclForm.FormState;
end;

function TDpiForm.GetModalResult: TModalResult;
begin
  Result := VclForm.ModalResult;
end;

function TDpiForm.GetOnClose: TCloseEvent;
begin
  Result := VclForm.OnClose;
end;

function TDpiForm.GetOnCreate: TNotifyEvent;
begin
  Result := VclForm.OnCreate;
end;

function TDpiForm.GetOnDeactivate: TNotifyEvent;
begin
  Result := VclForm.OnDeactivate;
end;

function TDpiForm.GetOnDestroy: TNotifyEvent;
begin
  Result := VclForm.OnDestroy;
end;

function TDpiForm.GetOnHide: TNotifyEvent;
begin
  Result := VclForm.OnHide;
end;

function TDpiForm.GetOnShow: TNotifyEvent;
begin
  Result := VclForm.OnShow;
end;

procedure TDpiForm.SetBorderIcons(AValue: TBorderIcons);
begin
  VclForm.BorderIcons := AValue;
end;

procedure TDpiForm.SetBorderStyle(AValue: TBorderStyle);
begin
  VclForm.BorderStyle := AValue;
end;

procedure TDpiForm.SetModalResult(AValue: TModalResult);
begin
  VclForm.ModalResult := AValue;
end;

procedure TDpiForm.SetOnClose(AValue: TCloseEvent);
begin
  VclForm.OnClose := AValue;
end;

procedure TDpiForm.SetOnCreate(AValue: TNotifyEvent);
begin
  VclForm.OnCreate := AValue;
end;

procedure TDpiForm.SetOnDeactivate(AValue: TNotifyEvent);
begin
  VclForm.OnDeactivate := AValue;
end;

procedure TDpiForm.SetOnDestroy(AValue: TNotifyEvent);
begin
  VclForm.OnDestroy := AValue;
end;

procedure TDpiForm.SetOnHide(AValue: TNotifyEvent);
begin
  VclForm.OnHide := AValue;
end;

procedure TDpiForm.SetOnShow(AValue: TNotifyEvent);
begin
  VclForm.OnShow := AValue;
end;

procedure TDpiForm.DoOnCreate;
begin
  if Assigned(VclForm.OnCreate) then
    VclForm.OnCreate(Self);
end;

procedure TDpiForm.CreateParams(var p: TCreateParams);
begin
  // TODO: VclForm.CreateParams(P);
end;

// This method is called by TWriter to retrieve the child components to write
procedure TDpiForm.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
begin
  DebugLn(['TDpiForm.GetChildren ComponentCount=', ComponentCount]);
  inherited GetChildren(Proc, Root);
  if Root = Self then begin
    for I := 0 to ComponentCount - 1 do begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
  end;
end;

function TDpiForm.GetVclCustomControl: TCustomControl;
begin
  Result := GetVclForm;
end;

function TDpiForm.GetVclForm: TForm;
begin
  if not Assigned(VclForm) then VclForm := TForm.Create(nil);
  Result := VclForm;
end;

function TDpiForm.ShowModal: Integer;
begin
  Result := GetVclForm.ShowModal;
end;

procedure TDpiForm.Close;
begin
  GetVclForm.Close;
end;

procedure TDpiForm.BringToFront;
begin
  GetVclForm.BringToFront;
end;

// Init the component with an IDE resource
constructor TDpiForm.Create(TheOwner: TComponent);
begin
  inherited;
  DebugLn(['TDpiForm.Create ', DbgSName(TheOwner)]);
  GlobalNameSpace.BeginWrite;
  try
    if (ClassType <> TDpiForm) and not (csDesigning in ComponentState)
    then begin
      if not InitResourceComponent(Self, TDataModule) then begin
        raise EResNotFound.Create('Resource missing for class ' + ClassName);
      end;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
  ScreenChanged;
  UpdateVclControl;
  DoOnCreate;
end;

destructor TDpiForm.Destroy;
begin
  FreeAndNil(VclForm);
end;

initialization

DpiScreen := TDpiScreen.Create;

finalization

FreeAndNil(DpiScreen);

end.

