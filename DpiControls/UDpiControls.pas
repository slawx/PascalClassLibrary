unit UDpiControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, FormEditingIntf, ProjectIntf,
  Controls, StdCtrls, fgl, Graphics;

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
    procedure FontChanged(Sender: TObject); override;
    procedure UpdateBounds; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetVisible(AValue: Boolean); override;
    procedure SetCaption(AValue: string); override;
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
    procedure FontChanged(Sender: TObject); override;
    procedure UpdateBounds; override;
    procedure SetVisible(AValue: Boolean); override;
    procedure SetCaption(AValue: string); override;
    procedure SetParent(AValue: TDpiWinControl); override;
  public
    VclButton: TButton;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
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
  RegisterComponents('DpiControls', [TDpiButton]);
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

procedure TDpiButton.FontChanged(Sender: TObject);
begin
  inherited;
  VclButton.Font.Size := Scale(Font.Size);
end;

procedure TDpiButton.UpdateBounds;
begin
  inherited;
  VclButton.SetBounds(Scale(Left), Scale(Top), Scale(Width), Scale(Height));
end;

procedure TDpiButton.SetVisible(AValue: Boolean);
begin
  inherited;
  VclButton.Visible := AValue;
end;

procedure TDpiButton.SetCaption(AValue: string);
begin
  inherited;
  VclButton.Caption := AValue;
end;

procedure TDpiButton.SetParent(AValue: TDpiWinControl);
begin
  inherited;
  if Assigned(Owner) and (Owner is TDpiForm) then
    VclButton.Parent := TDpiForm(Owner).VclForm;
end;

constructor TDpiButton.Create(TheOwner: TComponent);
begin
  inherited;
  VclButton := TButton.Create(nil);
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
end;

procedure TDpiControl.SetWidth(AValue: Integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
  UpdateBounds;
end;

procedure TDpiControl.ScreenChanged;
begin
  UpdateBounds;
  FontChanged(nil);
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
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TDpiControl.SetParent(AValue: TDpiWinControl);
begin
  if FParent = AValue then Exit;
  if Assigned(FParent) then FParent.Controls.Remove(Self);
  FParent := AValue;
  if Assigned(FParent) then FParent.Controls.Add(Self);
end;

procedure TDpiControl.SetFont(AValue: TDpiFont);
begin
  if FFont = AValue then Exit;
  FFont := AValue;
end;

procedure TDpiControl.FontChanged(Sender: TObject);
begin
end;

procedure TDpiControl.UpdateBounds;
begin
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

procedure TDpiForm.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
  VclForm.Visible := AValue;
end;

procedure TDpiForm.SetCaption(AValue: string);
begin
  inherited;
  VclForm.Caption:= AValue;
end;

procedure TDpiForm.SetOnShow(AValue: TNotifyEvent);
begin
  if FOnShow = AValue then Exit;
  FOnShow := AValue;
  VclForm.OnShow := AValue;
end;

procedure TDpiForm.FontChanged(Sender: TObject);
begin
  inherited;
  VclForm.Font.Size := Scale(Font.Size);
end;

procedure TDpiForm.UpdateBounds;
begin
  inherited;
  VclForm.SetBounds(Scale(Left), Scale(Top), Scale(Width), Scale(Height));
end;

// Init the component with an IDE resource
constructor TDpiForm.Create(TheOwner: TComponent);
begin
  VclForm := TForm.Create(nil);
  DebugLn(['TDpiForm.Create ', DbgSName(TheOwner)]);
  GlobalNameSpace.BeginWrite;
  try
    inherited Create(TheOwner);
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

