unit UDpiControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, FormEditingIntf, ProjectIntf,
  Controls, StdCtrls, fgl, Graphics, ComCtrls, ExtCtrls;

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
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetSize(AValue: Integer);
    procedure DoChange;
  protected
    procedure ScreenChanged;
  public
    VclFont: TFont;
    constructor Create;
    destructor Destroy; override;
  published
    property Size: Integer read FSize write SetSize;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TDpiWinControl = class;

  { TDpiControl }

  TDpiControl = class(TComponent)
  private
    FCaption: string;
    FFont: TDpiFont;
    FHeight: Integer;
    FLeft: Integer;
    FTop: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    FParent: TDpiWinControl;
    procedure SetFont(AValue: TDpiFont);
  protected
    procedure UpdateBounds; virtual;
    procedure FontChanged(Sender: TObject); virtual;
    procedure SetParent(AValue: TDpiWinControl); virtual;
    procedure SetCaption(AValue: string); virtual;
    procedure SetHeight(AValue: Integer); virtual;
    procedure SetLeft(AValue: Integer); virtual;
    procedure SetTop(AValue: Integer); virtual;
    procedure SetVisible(AValue: Boolean); virtual;
    procedure SetWidth(AValue: Integer); virtual;
    function GetVclControl: TControl; virtual;
  public
    procedure ScreenChanged; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure Show;
    procedure Hide;
    function Scale(Value: Integer): Integer;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Parent: TDpiWinControl read FParent write SetParent;
  published
    property Top: Integer read FTop write SetTop;
    property Left: Integer read FLeft write SetLeft;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Visible: Boolean read FVisible write SetVisible;
    property Caption: string read FCaption write SetCaption;
    property Font: TDpiFont read FFont write SetFont;
  end;

  TDpiControls = specialize TFPGObjectList<TDpiControl>;

  { TDpiWinControl }

  TDpiWinControl = class(TDpiControl)
  private
  protected
    function GetVclWinControl: TWinControl; virtual;
  public
    Controls: TDpiControls;
    procedure ScreenChanged; override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  { TDpiForm }

  TDpiForm = class(TDpiWinControl)
  private
    FOnShow: TNotifyEvent;
    procedure SetOnShow(AValue: TNotifyEvent);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetVclControl: TControl; override;
    function GetVclWinControl: TWinControl; override;
  public
    VclForm: TForm;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnShow: TNotifyEvent read FOnShow write SetOnShow;
  end;

  TDpiForms = specialize TFPGObjectList<TDpiForm>;

  { TDpiButton }

  TDpiButton = class(TDpiControl)
  private
    FOnClick: TNotifyEvent;
    procedure SetOnClick(AValue: TNotifyEvent);
  protected
    function GetVclControl: TControl; override;
  public
    VclButton: TButton;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
  end;

  { TDpiImage }

  TDpiImage = class(TDpiControl)
  private
    FStretch: Boolean;
    procedure SetStretch(AValue: Boolean);
  protected
  public
    VclImage: TImage;
    function GetVclControl: TControl; override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Stretch: Boolean read FStretch write SetStretch;
  end;

  { TDpiScreen }

  TDpiScreen = class
  private
    FDpi: Integer;
    procedure SetDpi(AValue: Integer);
    procedure UpdateForms;
  public
    Forms: TDpiForms;
    constructor Create;
    destructor Destroy; override;
  published
    property Dpi: Integer read FDpi write SetDpi;
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
  RegisterComponents('DpiControls', [TDpiButton, TDpiImage]);
end;

{ TDpiImage }

procedure TDpiImage.SetStretch(AValue: Boolean);
begin
  if FStretch = AValue then Exit;
  FStretch := AValue;
  VclImage.Stretch := AValue;
end;

function TDpiImage.GetVclControl: TControl;
begin
  Result := VclImage;
end;

constructor TDpiImage.Create(TheOwner: TComponent);
begin
  inherited;
  VclImage := TImage.Create(nil);
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

procedure TDpiFont.ScreenChanged;
begin
  DoChange;
end;

procedure TDpiFont.SetOnChange(AValue: TNotifyEvent);
begin
  if FOnChange = AValue then Exit;
  FOnChange := AValue;
end;

constructor TDpiFont.Create;
begin
  VclFont := TFont.Create;
  Size := 8;
end;

destructor TDpiFont.Destroy;
begin
  FreeAndNil(VclFont);
  inherited Destroy;
end;

{ TDpiWinControl }

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

constructor TDpiWinControl.Create(TheOwner: TComponent);
begin
  inherited;
  Controls := TDpiControls.Create;
  Controls.FreeObjects := False;
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

procedure TDpiButton.SetOnClick(AValue: TNotifyEvent);
begin
  if FOnClick = AValue then Exit;
  FOnClick := AValue;
  VclButton.OnClick := AValue;
end;

function TDpiButton.GetVclControl: TControl;
begin
  Result := VclButton;
end;

constructor TDpiButton.Create(TheOwner: TComponent);
begin
  inherited;
  VclButton := TButton.Create(nil);
  ScreenChanged;
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
  if FVisible = AValue then Exit;
  FVisible := AValue;
  GetVclControl.Visible := AValue;;
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

function TDpiControl.Scale(Value: Integer): Integer;
begin
  Scale := Round(Value * DpiScreen.Dpi / 96);
end;

constructor TDpiControl.Create(TheOwner: TComponent);
begin
  inherited;
  FFont := TDpiFont.Create;
  FFont.OnChange := @FontChanged;
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
  if FCaption = AValue then Exit;
  FCaption := AValue;
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

procedure TDpiControl.FontChanged(Sender: TObject);
begin
  GetVclControl.Font.Size := Scale(Font.Size);
end;

procedure TDpiControl.UpdateBounds;
begin
  GetVclControl.SetBounds(Scale(Left), Scale(Top), Scale(Width), Scale(Height));
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

function TDpiForm.GetVclControl: TControl;
begin
  Result := VclForm;
end;

function TDpiForm.GetVclWinControl: TWinControl;
begin
  Result := VclForm;
end;

procedure TDpiForm.SetOnShow(AValue: TNotifyEvent);
begin
  if FOnShow = AValue then Exit;
  FOnShow := AValue;
  VclForm.OnShow := AValue;
end;

// Init the component with an IDE resource
constructor TDpiForm.Create(TheOwner: TComponent);
begin
  inherited;
  VclForm := TForm.Create(nil);
  ScreenChanged;
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

