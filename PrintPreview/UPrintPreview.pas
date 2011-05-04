unit UPrintPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, ActnList, PrintersDlgs, Contnrs, Printers, StdCtrls;

const
  ScreenDPI = 72;
  SizeDivider = 7.2;

type
  TPrintPreviewForm = class;

  { TPrintPage }

  TPrintPage = class
    Bitmap: TBitmap;
    constructor Create;
    destructor Destroy; override;
  end;

  { TPrintPreview }

  TPrintPreview = class(TComponent)
  private
    FOnPrint: TNotifyEvent;
    FOnPrintFooter: TNotifyEvent;
    FZoom: Double;
    FPageNumber: Integer;
    FPageCount: Integer;
    function GetHeight: Integer;
    function GetPageCount: Integer;
    function GetPageNumber: Integer;
    function GetWidth: Integer;
    function GetXDPI: Integer;
    function GetYDPI: Integer;
    function GetZoom: Double;
    procedure SetZoom(const AValue: Double);
    procedure UpdateMargins;
  public
    Canvas: TCanvas;
    Pages: TObjectList;
    PageTitle: string;
    Margins: TRect;
    property XDPI: Integer read GetXDPI;
    property YDPI: Integer read GetYDPI;
    function MM(AValue: Double; VertRes: Boolean=True): Integer;
    procedure CreateNewPage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Preview;
    procedure Execute;
    procedure Print;
  published
    property PageNumber: Integer read GetPageNumber;
    property Zoom: Double read GetZoom write SetZoom;
    property OnPrint: TNotifyEvent read FOnPrint write FOnPrint;
    property PageWidth: Integer read GetWidth;
    property PageHeight: Integer read GetHeight;
    property PageCount: Integer read GetPageCount;
  end;

  { TPrintPreviewForm }

  TPrintPreviewForm = class(TForm)
    AClose: TAction;
    ALastPage: TAction;
    AFirstPage: TAction;
    ANextPage: TAction;
    APreviousPage: TAction;
    AZoomOut: TAction;
    AZoomIn: TAction;
    APageSetup: TAction;
    APrinterSetup: TAction;
    APrint: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    EditPageNumber: TEdit;
    Image1: TImage;
    ImageList1: TImageList;
    PageSetupDialog1: TPageSetupDialog;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    ScrollBarHoriz: TScrollBar;
    ScrollBarVert: TScrollBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure ACloseExecute(Sender: TObject);
    procedure AFirstPageExecute(Sender: TObject);
    procedure ALastPageExecute(Sender: TObject);
    procedure ANextPageExecute(Sender: TObject);
    procedure APageSetupExecute(Sender: TObject);
    procedure APreviousPageExecute(Sender: TObject);
    procedure APrinterSetupExecute(Sender: TObject);
    procedure APrintExecute(Sender: TObject);
    procedure AZoomInExecute(Sender: TObject);
    procedure AZoomOutExecute(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure EditPageNumberChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ScrollBarHorizChange(Sender: TObject);
    procedure ScrollBarVertChange(Sender: TObject);
  private
    FPrintPreview: TPrintPreview;
    procedure ReloadPageNumber;
  public
    PageNumber: Integer;
    procedure Redraw;
    property PrintPreview: TPrintPreview read FPrintPreview
      write FPrintPreview;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TPrintPreview]);
end;

{ TPrintPage }

constructor TPrintPage.Create;
begin
  Bitmap := TBitmap.Create;
end;

destructor TPrintPage.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

{ TPrintPreview }

function TPrintPreview.GetHeight: Integer;
begin
  if Printer.Printing then Result := Printer.PageHeight
    else Result := Round(Printer.PageHeight / SizeDivider * Zoom);
end;

function TPrintPreview.GetPageCount: Integer;
begin
  Result := FPageCount;
end;

function TPrintPreview.GetPageNumber: Integer;
begin
  if Printer.Printing then Result := Printer.PageNumber
    else Result := FPageNumber + 1;
end;

function TPrintPreview.GetWidth: Integer;
begin
  if Printer.Printing then Result := Printer.PageWidth
    else Result := Round(Printer.PageWidth / SizeDivider * Zoom);
end;

function TPrintPreview.GetXDPI: Integer;
begin
  if Printer.Printing then Result := Printer.XDPI
    else Result := Round(ScreenDPI * Zoom);
end;

function TPrintPreview.GetYDPI: Integer;
begin
  if Printer.Printing then Result := Printer.YDPI
    else Result := Round(ScreenDPI * Zoom);
end;

function TPrintPreview.GetZoom: Double;
begin
  if Printer.Printing then Result := 1
    else Result := FZoom;
end;

procedure TPrintPreview.SetZoom(const AValue: Double);
begin
  if FZoom = AValue then Exit;
  FZoom := AValue;
  Preview;
end;

procedure TPrintPreview.UpdateMargins;
begin
  Margins := Rect(MM(10), MM(10), MM(10), MM(10));
end;

function TPrintPreview.MM(AValue: Double; VertRes: Boolean = True): Integer;
begin
  if VertRes then
    Result := Round(AValue * YDPI / 25.4)
  else
    Result := Round(AValue * XDPI / 25.4);
end;

procedure TPrintPreview.CreateNewPage;
var
  NewPage: TPrintPage;
begin
  if Printer.Printing then begin
    Printer.NewPage;
  end else begin
      NewPage := TPrintPage.Create;
      Pages.Add(NewPage);
      Canvas := NewPage.Bitmap.Canvas;
      NewPage.Bitmap.SetSize(PageWidth, PageHeight);
      with NewPage.Bitmap.Canvas do begin
        Brush.Color := clWhite;
        Brush.Style := bsSolid;
        Clear;                           //Brush.Color := clWhite;
        Brush.Color := clWhite;
        Brush.Style := bsSolid;
        Clear;                           //Brush.Color := clWhite;
        //Brush.Style := bsSolid;
        //FillRect(Rect(0, 0, Width, Height));
        //Brush.Color := clRed;
        //Brush.Style := bsSolid;
        //FillRect(Rect(0, 0, 100, 100));
        Inc(FPageNumber);
      end;
    end;
end;

constructor TPrintPreview.Create(AOwner: TComponent);
begin
  inherited;
  Zoom := 1;
  Pages := TObjectList.Create;
end;

destructor TPrintPreview.Destroy;
begin
  Pages.Free;
  inherited Destroy;
end;

procedure TPrintPreview.Preview;
const
  DefaultMargin = 10;
begin
  if Assigned(FOnPrint) then begin
    Pages.Clear;
    FPageNumber := -1;
    UpdateMargins;
    CreateNewPage;
    FOnPrint(Self);
  end;
end;

procedure TPrintPreview.Execute;
var
  Form: TPrintPreviewForm;
begin
  try
    Form := TPrintPreviewForm.Create(nil);
    Form.PrintPreview := Self;
    Preview;
    FPageCount := Pages.Count;
    Preview; // Call again for page count update
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure TPrintPreview.Print;
begin
  if Assigned(FOnPrint) then begin
    Canvas := Printer.Canvas;
    try
      FPageNumber := 0;
      Canvas := Printer.Canvas;
      Printer.Title := UTF8Decode(PageTitle);
      Printer.BeginDoc;
      UpdateMargins;
      FOnPrint(Self);
    finally
      Printer.EndDoc;
    end;
  end;
end;


{ TPrintPreviewForm }

procedure TPrintPreviewForm.APrintExecute(Sender: TObject);
begin
  PrintDialog1.MinPage := 1;
  PrintDialog1.MaxPage := PrintPreview.PageCount;
  PrintDialog1.FromPage := 1;
  PrintDialog1.ToPage := PrintPreview.PageCount;
  if PrintDialog1.Execute then
    if Assigned(FPrintPreview) then PrintPreview.Print;
end;

procedure TPrintPreviewForm.AZoomInExecute(Sender: TObject);
begin
  PrintPreview.Zoom := PrintPreview.Zoom * 2;
  Redraw;
end;

procedure TPrintPreviewForm.AZoomOutExecute(Sender: TObject);
begin
  PrintPreview.Zoom := PrintPreview.Zoom / 2;
  Redraw;
end;

procedure TPrintPreviewForm.Button5Click(Sender: TObject);
begin
  ShowMessage('Printer page size: ' + IntToStr(Printer.PageWidth) + ', ' + IntToStr(Printer.PageHeight) +
    ' DPI: ' + IntToStr(Printer.XDPI) + ', ' + IntToStr(Printer.YDPI) +
  ' Page size: ' + IntToStr(TPrintPage(PrintPreview.Pages.Last).Bitmap.Width) +
    ', ' + IntToStr(TPrintPage(PrintPreview.Pages.Last).Bitmap.Height) + ' ');
end;

procedure TPrintPreviewForm.EditPageNumberChange(Sender: TObject);
var
  Value: Integer;
begin
  if TryStrToInt(EditPageNumber.Text, Value) then begin
    PageNumber := Value;
    if PageNumber < 0 then PageNumber := 0;
    if PageNumber >= PrintPreview.PageCount then
      PageNumber := PrintPreview.PageCount - 1;
  end;
  ReloadPageNumber;
end;

procedure TPrintPreviewForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
end;

procedure TPrintPreviewForm.FormDestroy(Sender: TObject);
begin
end;

procedure TPrintPreviewForm.FormResize(Sender: TObject);
begin
  Redraw;
end;

procedure TPrintPreviewForm.FormShow(Sender: TObject);
begin
  PrintDialog1.MaxPage := PrintPreview.Pages.Count;
  ReloadPageNumber;
  Redraw;
end;

procedure TPrintPreviewForm.ScrollBarHorizChange(Sender: TObject);
begin
  Redraw;
end;

procedure TPrintPreviewForm.ScrollBarVertChange(Sender: TObject);
begin
  Redraw;
end;

procedure TPrintPreviewForm.ReloadPageNumber;
begin
  EditPageNumber.Text := IntToStr(PageNumber);
  Redraw;
  ANextPage.Enabled := PageNumber < (PrintPreview.PageCount - 1);
  APreviousPage.Enabled := PageNumber > 0;
end;

procedure TPrintPreviewForm.Redraw;
var
  SourceRect: TRect;
  DestRect: TRect;
  Page: TPrintPage;
begin
  Page := TPrintPage(PrintPreview.Pages[PageNumber]);
  SourceRect := Rect(0, 0,
    Page.Bitmap.Canvas.Width,
    Page.Bitmap.Canvas.Height);
  DestRect.Left := -Round(ScrollBarHoriz.Position / ScrollBarHoriz.Max * Width);
  DestRect.Top := -Round(ScrollBarVert.Position / ScrollBarVert.Max * Height);
  DestRect.Right := DestRect.Left + Round(Page.Bitmap.Canvas.Width * PrintPreview.Zoom);
  DestRect.Bottom := DestRect.Top + Round(Page.Bitmap.Canvas.Height * PrintPreview.Zoom);
  try
    Image1.Picture.Bitmap.SetSize(Image1.Width, Image1.Height);
    Image1.Picture.Bitmap.BeginUpdate(True);
    with Image1.Picture.Bitmap, Canvas do begin
      Brush.Color:= clBlue;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, Width, Height));
      Draw(DestRect.Left, DestRect.Top, Page.Bitmap);
      //CopyRect(DestRect, Page.Bitmap.Canvas, SourceRect);
      Pen.Style := psSolid;
      Pen.Color := clBlack;
      Frame(DestRect);
    end;
  finally
    Image1.Picture.Bitmap.EndUpdate;
  end;
end;

procedure TPrintPreviewForm.APageSetupExecute(Sender: TObject);
begin
  PageSetupDialog1.Margins := PrintPreview.Margins;
  PageSetupDialog1.Execute;
  PrintPreview.Margins := PageSetupDialog1.Margins;
end;

procedure TPrintPreviewForm.APreviousPageExecute(Sender: TObject);
begin
  Dec(PageNumber);
  if PageNumber < 0 then PageNumber := 0;
  ReloadPageNumber;
end;

procedure TPrintPreviewForm.ANextPageExecute(Sender: TObject);
begin
  Inc(PageNumber);
  if PageNumber >= PrintPreview.PageCount then
    PageNumber := PrintPreview.PageCount;
  ReloadPageNumber;
end;

procedure TPrintPreviewForm.ALastPageExecute(Sender: TObject);
begin
  PageNumber := PrintPreview.PageCount - 1;
  ReloadPageNumber;
end;

procedure TPrintPreviewForm.AFirstPageExecute(Sender: TObject);
begin
  PageNumber := 0;
  ReloadPageNumber;
end;

procedure TPrintPreviewForm.ACloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TPrintPreviewForm.APrinterSetupExecute(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

initialization
  {$I UPrintPreview.lrs}

end.

