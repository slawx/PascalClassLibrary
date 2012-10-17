unit UCDManager;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UCDCommon, Controls, Contnrs, Dialogs,
  UCDPopupMenu, LCLType, LCLIntf, LMessages, Graphics, Buttons,
  UCDConjoinForm, Menus, ExtCtrls, Forms;

const
  GrabberSize = 22;

type
  TCDManager = class;
  TCDManagerItem = class;

  TCDPanelForm = class(TForm)
    Panel: TPanel;
  end;

  { TCDHeaderButton }

  TCDHeaderButton = class
    Icon: TImage;
    Visible: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { TCDHeader }

  TCDHeader = class(TPanel)
  private
    MyFont: hFont;
    procedure CloseButtonClick(Sender: TObject);
    procedure PaintExecute(Sender: TObject);
    procedure RearrangeButtons;
  public
    Buttons: TObjectList; // TList<TCDHeaderButton>
    Icon: TImage;
    Control: TControl;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TCDPanelHeader }

  TCDPanelHeader = class(TPanel)
  private
    FHeaderPos: THeaderPos;
    function GetHeaderVisible: Boolean;
    procedure SetHeaderPos(const AValue: THeaderPos);
    procedure SetHeaderVisible(const AValue: Boolean);
  public
    Header: TCDHeader;
    ControlPanel: TPanel;
    DockItem: TCDManagerItem;
    property HeaderPos: THeaderPos read FHeaderPos write SetHeaderPos;
    property HeaderVisible: Boolean read GetHeaderVisible write SetHeaderVisible;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TCDManagerItem }

  TCDManagerItem = class
  private
    FControl: TWinControl;
    procedure ResizeExecute(Sender: TObject);
  public
    Manager: TCDManager;
    procedure SetControl(const AValue: TWinControl); virtual;
    procedure DockPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Paint(Sender: TObject); virtual;
    procedure VisibleChange(Sender: TObject); virtual;
    procedure VisibleChanging(Sender: TObject); virtual;
    constructor Create; virtual;
    destructor Destroy; override;
    property Control: TWinControl read FControl write SetControl;
  end;

  { TCDManager }

  TCDManager = class(TCDManagerBase)
  protected
    FUpdateCount: Integer;
    FDockStyle: TCDStyleType;
    procedure SetHeaderPos(const AValue: THeaderPos); virtual;
  private
    FDockSite: TWinControl;
    FDockSiteVisible: Boolean;
    FHeaderPos: THeaderPos;
    FHeaderVisible: Boolean;
    FOnDockSiteHide: TNotifyEvent;
    FOnDockSiteShow: TNotifyEvent;
    function GetDockSite: TWinControl;
    function GetMoveDuration: Integer;
    procedure SetDockSiteVisible(AValue: Boolean); virtual;
    procedure SetDockStyle(const AValue: TCDStyleType);
    procedure SetHeaderVisible(const AValue: Boolean);
    procedure SetMoveDuration(const AValue: Integer);
    procedure CloseHandler(Sender: TObject; var CloseAction: TCloseAction);
  public
    Locked: Boolean;
    PopupMenu: TCDPopupMenu;
    FreeParentIfEmpty: Boolean; // Free or not parent conjoin forms
    procedure SetVisible(const AValue: Boolean); virtual;
    constructor Create(ADockSite: TWinControl); override;
    destructor Destroy; override;
    procedure Update; virtual;
    procedure Switch(Index: Integer); virtual;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean); virtual;
    procedure Assign(Source: TCDManager); virtual;
    procedure InsertControlPanel(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); virtual;
    function GetHeaderPos: THeaderPos; virtual;
    procedure BringToFront; virtual;

    // Inherited from TDockManager
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure GetControlBounds(Control: TControl;
      out AControlBounds: TRect); override;
    function GetDockEdge(ADockObject: TDragDockObject): boolean; override;
    procedure InsertControl(ADockObject: TDragDockObject); override; overload;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); override; overload;
    procedure LoadFromStream(Stream: TStream); override;
    procedure PaintSite(DC: HDC); override;
    procedure MessageHandler(Sender: TControl; var Message: TLMessage); override;
    procedure PositionDockRect(ADockObject: TDragDockObject); override; overload;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect); override; overload;
    procedure RemoveControl(Control: TControl); override;
    procedure ResetBounds(Force: Boolean); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SetReplacingControl(Control: TControl); override;
    function AutoFreeByControl: Boolean; override;

    function FindControlInPanels(Control: TControl): TCDManagerItem; virtual;
    function CreateConjoinForm: TCDConjoinForm;
    function CreateDockableForm: TCDPanelForm;
    property DockStyle: TCDStyleType read FDockStyle write SetDockStyle;
    property MoveDuration: Integer read GetMoveDuration write SetMoveDuration;
    property DockSite: TWinControl read GetDockSite;
    property HeaderPos: THeaderPos read GetHeaderPos write SetHeaderPos;
    property HeaderVisible: Boolean read FHeaderVisible write SetHeaderVisible;
    property Visible: Boolean write SetVisible;
    property DockSiteVisible: Boolean read FDockSiteVisible write SetDockSiteVisible;
    property OnDockSiteHide: TNotifyEvent read FOnDockSiteHide write FOnDockSiteHide;
    property OnDockSiteShow: TNotifyEvent read FOnDockSiteShow write FOnDockSiteShow;
  end;


implementation

uses
  UCDManagerRegions, UCDManagerTabs, UCDManagerRegionsPopup, UCDManagerTabsPopup,
  UCDResource, UCDClient;

function CreateRotatedFont(F: TFont; Angle: Integer): Integer;
var
  LF: TLogFont;
begin
  FillChar(LF, SizeOf(LF), #0);
  with LF do begin
    lfHeight := F.Height;
    lfWidth := 0;
    lfEscapement := Angle * 10;
    lfOrientation := 0;
    if fsBold in F.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Byte(fsItalic in F.Style);
    lfUnderline := Byte(fsUnderline in F.Style);
    lfStrikeOut := Byte(fsStrikeOut in F.Style);
    lfCharSet := DEFAULT_CHARSET;
    StrPCopy(lfFaceName, F.Name);
    lfQuality := DEFAULT_QUALITY;
    {everything else as default}
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case F.Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  Result := CreateFontIndirect(LF);
end;

{ TCDHeaderButton }

constructor TCDHeaderButton.Create;
begin
  inherited;
  Icon := TImage.Create(nil);
end;

destructor TCDHeaderButton.Destroy;
begin
  Icon.Free;
  inherited Destroy;
end;

{ TCDPanelHeader }

procedure TCDPanelHeader.SetHeaderPos(const AValue: THeaderPos);
begin
  if FHeaderPos=AValue then exit;
  FHeaderPos:=AValue;

  //Paint(Self);
end;

function TCDPanelHeader.GetHeaderVisible: Boolean;
begin
  Result := Header.Visible;
end;

procedure TCDPanelHeader.SetHeaderVisible(const AValue: Boolean);
begin
  Header.Visible := AValue;
end;

constructor TCDPanelHeader.Create(TheOwner: TComponent);
begin
  inherited;
  //Paint.OnPaint := Paint;
//  Header.Shape.OnMouseDown := DockPanelMouseDown;
//  Header.Title.OnMouseDown := DockPanelMouseDown;
  HeaderPos := hpTop;
  Constraints.MinHeight := GrabberSize;
  Align := alClient;

  ControlPanel := TPanel.Create(Self);
  with ControlPanel do begin
    Parent := Self;
    Visible := True;
    DockSite := True;
    UseDockManager := True;
    Align := alClient;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    //Color := clGreen;
  end;
  Header := TCDHeader.Create(Self);
  with Header do begin
    Parent := Self;
    Visible := True;
    Align := alTop;
    Height := GrabberSize;
    //ManagerItem := Self;
  end;
  //OnResize := ResizeExecute;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  HeaderVisible := True;
end;

destructor TCDPanelHeader.Destroy;
begin
  inherited Destroy;
end;

{ TCDManagerItem }

procedure TCDManagerItem.Paint(Sender: TObject);
begin
end;

constructor TCDManagerItem.Create;
begin

end;

procedure TCDManagerItem.ResizeExecute(Sender: TObject);
begin
(*  if Assigned(Control) then begin
    Control.Top := GrabberSize;
    Control.Left := 0;
    Control.Width := Width;
    Control.Height := Height - GrabberSize;
    //Control.SetBounds(0, GrabberSize, Width - Control.Left,
    //  Height - Control.Top);
  end;*)
end;

procedure TCDManagerItem.SetControl(const AValue: TWinControl);
begin
  if FControl = AValue then Exit;
  FControl := AValue;
end;

procedure TCDManagerItem.DockPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Control is TForm then begin
    TForm(Control).SetFocus;
    Paint(Self);
  end;
  if (Button = mbLeft) then begin
    //(Control as TWinControl).DockSite := False;
    //ClientAreaPanel.DockSite := False;
    if not Manager.Locked then begin
      (Control as TWinControl).BeginDrag(False, 10);
      DragManager.DragStart(Control, False, 1);
    end;
  end;
end;

destructor TCDManagerItem.Destroy;
begin
  if Assigned(Control) then
    Control.RemoveHandlerOnVisibleChanged(VisibleChange);
  inherited Destroy;
end;

procedure TCDManagerItem.VisibleChange(Sender: TObject);
begin
end;

procedure TCDManagerItem.VisibleChanging(Sender: TObject);
begin

end;

{ TCDManager }

function TCDManager.FindControlInPanels(Control: TControl
  ): TCDManagerItem;
begin
  Result := nil;
end;

function TCDManager.GetDockSite: TWinControl;
begin
  Result := FDockSite;
end;

function TCDManager.GetHeaderPos: THeaderPos;
begin
  Result := FHeaderPos;
end;

procedure TCDManager.BringToFront;
begin
  //DockSiteVisible := True;
  DockSite.Show;
  DockSite.BringToFront;
  if Assigned(DockSite.Parent) then begin
    if Assigned(DockSite.Parent.DockManager)
      and (DockSite.Parent.DockManager is TCDManager) then
    TCDManager(DockSite.Parent.DockManager).BringToFront
    else DockSite.Parent.BringToFront;
  end;
end;

function TCDManager.GetMoveDuration: Integer;
begin

end;

procedure TCDManager.SetDockSiteVisible(AValue: Boolean);
begin
  if FDockSiteVisible = AValue then Exit;
  FDockSiteVisible := AValue;
  SetVisible(FDockSiteVisible);
  if Assigned(FOnDockSiteHide) and (not FDockSiteVisible) then
    FOnDockSiteHide(Self);
  if Assigned(FOnDockSiteShow) and FDockSiteVisible then
    FOnDockSiteShow(Self);
end;

constructor TCDManager.Create(ADockSite: TWinControl);
var
  NewMenuItem: TMenuItem;
  NewMenuItem2: TMenuItem;
begin
  inherited Create(ADockSite);

  FDockSite := ADockSite;

  FreeParentIfEmpty := True;

  FDockStyle := dsList; // dsNone
  FHeaderVisible := True;
  PopupMenu := TCDPopupMenu.Create(Self);
  PopupMenu.Parent := ADockSite;
  if ADockSite is TForm then
    TForm(ADockSite).AddHandlerClose(CloseHandler);
end;

destructor TCDManager.Destroy;
begin
  if FDockSite is TForm then
    TForm(FDockSite).RemoveHandlerClose(CloseHandler);
  PopupMenu.Free;
  inherited Destroy;
end;

procedure TCDManager.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCDManager.EndUpdate;
begin
  if FUpdateCount > 0 then Dec(FUpdateCount);
  if FUpdateCount = 0 then Update;
end;

procedure TCDManager.GetControlBounds(Control: TControl; out
  AControlBounds: TRect);
begin
end;

function TCDManager.GetDockEdge(ADockObject: TDragDockObject): boolean;
begin
  Result := inherited GetDockEdge(ADockObject);
end;

procedure TCDManager.InsertControl(ADockObject: TDragDockObject);
begin
  inherited InsertControl(ADockObject);
end;

procedure TCDManager.InsertControlPanel(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
begin
end;

procedure TCDManager.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewSplitter: TSplitter;
  NewDockPanel: TCDManagerItem;
  NewPanel: TPanel;
  I: Integer;
  NewConjoinDockForm: TCDConjoinForm;
  NewDockSite: TWinControl;
  NewForm: TForm;
begin
  if (FDockSite is TForm) and (not (FDockSite is TCDConjoinForm)) then begin
    if (not Assigned(FDockSite.Parent)) then begin
      // Create conjointed form
      NewConjoinDockForm := CreateConjoinForm;
      FDockSite.ManualDock(NewConjoinDockForm);
      Control.ManualDock(NewConjoinDockForm, nil, InsertAt);
      NewConjoinDockForm.UpdateCaption;
    end else begin
      NewConjoinDockForm := CreateConjoinForm;
      NewDockSite := FDockSite.HostDockSite;
//      FDockSite.ManualFloat(FDockSite.BoundsRect);
      NewConjoinDockForm.ManualDock(NewDockSite, nil, InsertAt);
      FDockSite.ManualDock(NewConjoinDockForm);
      Control.ManualDock(NewConjoinDockForm, nil, InsertAt);
      NewConjoinDockForm.UpdateCaption;
    end;
  end else
  if (FDockSite is TCDConjoinForm) or (FDockSite is TPanel)  then begin
    InsertControlPanel(Control, InsertAt, DropCtl);
  end;
  if FDockSite is TCDConjoinForm then
    TCDConjoinForm(FDockSite).UpdateCaption;

//  FDockPanel.Invalidate;
  //inherited;
end;

procedure TCDManager.LoadFromStream(Stream: TStream);
begin
end;

procedure TCDManager.PaintSite(DC: HDC);
begin
end;

procedure TCDManager.MessageHandler(Sender: TControl;
  var Message: TLMessage);
begin
  inherited MessageHandler(Sender, Message);
end;

procedure TCDManager.PositionDockRect(ADockObject: TDragDockObject);
begin
  inherited PositionDockRect(ADockObject);
end;

procedure TCDManager.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
begin
  case DropAlign of
    alNone: begin
      DockRect := Rect(0, 0, FDockSite.ClientWidth, FDockSite.ClientHeight);
    end;
    alRight: begin
      DockRect := Rect(FDockSite.ClientWidth div 2, 0, FDockSite.ClientWidth, FDockSite.ClientHeight);
    end;
    alLeft: begin
      DockRect := Rect(0, 0, FDockSite.ClientWidth div 2, FDockSite.ClientHeight);
    end;
    alTop: begin
      DockRect := Rect(0, 0, FDockSite.ClientWidth, FDockSite.ClientHeight div 2);
    end;
    alBottom: begin
      DockRect := Rect(0, FDockSite.ClientHeight div 2, FDockSite.ClientWidth, FDockSite.ClientHeight);
    end;
  end;
  DockRect.TopLeft := FDockSite.ClientToScreen(DockRect.TopLeft);
  DockRect.BottomRight := FDockSite.ClientToScreen(DockRect.BottomRight);
end;

procedure TCDManager.RemoveControl(Control: TControl);
begin
  if FDockSite is TCDConjoinForm then
    TCDConjoinForm(FDockSite).UpdateCaption;
end;

procedure TCDManager.ResetBounds(Force: Boolean);
var
  I: Integer;
  Control: TControl;
  R: TRect;
begin
end;

procedure TCDManager.SaveToStream(Stream: TStream);
begin
end;

procedure TCDManager.SetReplacingControl(Control: TControl);
begin
  inherited SetReplacingControl(Control);
end;

function TCDManager.AutoFreeByControl: Boolean;
begin
  Result := inherited AutoFreeByControl;
end;

function TCDManager.CreateConjoinForm: TCDConjoinForm;
var
  NewDockSite: TWinControl;
  NewConjoinDockForm: TCDConjoinForm;
begin
  NewConjoinDockForm := TCDConjoinForm.Create(Application);
  NewConjoinDockForm.Name := GetUniqueName('ConjoinForm');
  NewConjoinDockForm.Visible := True;
  NewConjoinDockForm.BoundsRect := FDockSite.BoundsRect;
  NewConjoinDockForm.CoolDockClient.Master := Self.Master;
  NewDockSite := FDockSite.HostDockSite;
  // FDockSite.ManualFloat(FDockSite.BoundsRect);
  //NewConjoinDockForm.ManualDock(NewDockSite, nil, InsertAt);
  Result := NewConjoinDockForm;
end;

function TCDManager.CreateDockableForm: TCDPanelForm;
var
  NewClient: TCDClient;
begin
  Application.CreateForm(TCDPanelForm, Result);
  Result.Name := GetUniqueName('DockForm');
  NewClient := TCDClient.Create(Result);
  Result.Panel := TPanel.Create(Result);
  Result.Panel.Parent := Result;
  //Result.Panel.Visible := True;
  Result.Panel.BevelInner := bvNone;
  Result.Panel.BevelOuter := bvNone;
  NewClient.Panel := Result.Panel;
  NewClient.Master := Self.Master;
  NewClient.Dockable := False;
end;

procedure TCDManager.SetDockStyle(const AValue: TCDStyleType);
var
  I: Integer;
  NewManager: TCDManager;
begin
  if FDockStyle <> AValue then begin
    FDockStyle := AValue;
    if AValue = dsTabs then begin
      NewManager := TCDManagerTabs.Create(FDockSite);
    end else
    if AValue = dsList then begin
      NewManager := TCDManagerRegions.Create(FDockSite);
    end else
    if AValue = dsPopupList then begin
      NewManager := TCDManagerPopupRegions.Create(FDockSite);
    end else
    if AValue = dsPopupTabs then begin
      NewManager := TCDManagerTabsPopup.Create(FDockSite);
    end;
    if DockSite.DockManager is TCDManager then
      NewManager.Assign(TCDManager(DockSite.DockManager));
    DockSite.DockManager := NewManager;
    NewManager.Update;
  end;
end;

procedure TCDManager.SetHeaderVisible(const AValue: Boolean);
begin
  if FHeaderVisible = AValue then Exit;
  FHeaderVisible := AValue;
  if Assigned(DockSite.HostDockSite) then
    TCDManager(DockSite.HostDockSite.DockManager).Update;
end;

procedure TCDManager.SetHeaderPos(const AValue: THeaderPos);
begin
  FHeaderPos := AValue;
end;

procedure TCDManager.SetMoveDuration(const AValue: Integer);
begin
end;

procedure TCDManager.CloseHandler(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  //DockSite.Visible := False;
  //SetVisible(FDockSite.Visible);
end;

procedure TCDManager.SetVisible(const AValue: Boolean);
begin
end;

procedure TCDManager.Update;
begin
end;

procedure TCDManager.Switch(Index: Integer);
begin

end;

procedure TCDManager.ChangeVisible(Control: TWinControl; Visible: Boolean);
begin

end;

procedure TCDManager.Assign(Source: TCDManager);
begin
  FDockStyle := Source.FDockStyle;
  FDockSite := Source.FDockSite;
end;

{ TCDHeader }

constructor TCDHeader.Create(TheOwner: TComponent);
var
  NewButton: TCDHeaderButton;
begin
  inherited Create(TheOwner);
  OnPaint := PaintExecute;
  //MyFont := CreateRotatedFont(Canvas.Font, 90);

  Buttons := TObjectList.Create;

  NewButton := TCDHeaderButton.Create;
  with NewButton do begin
    DataModule2.ImageList1.GetBitmap(0, Icon.Picture.Bitmap);
    Icon.Parent := Self;
    Icon.OnClick := CloseButtonClick;
    Visible := True;
  end;
  Buttons.Add(NewButton);
  NewButton := TCDHeaderButton.Create;
  with NewButton do begin
    DataModule2.ImageList1.GetBitmap(1, Icon.Picture.Bitmap);
    Icon.Parent := Self;
    Icon.OnClick := nil;
    Visible := False;
  end;
  Buttons.Add(NewButton);
  NewButton := TCDHeaderButton.Create;
  with NewButton do begin
    DataModule2.ImageList1.GetBitmap(2, Icon.Picture.Bitmap);
    Icon.Parent := Self;
    Icon.OnClick := nil;
    Visible := False;
  end;
  Buttons.Add(NewButton);
  RearrangeButtons;

  Icon := TImage.Create(Self);
  with Icon do begin
    Parent := Self;
    Left := 4;
    Top := 2;
    Visible := True;
  end;

  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

destructor TCDHeader.Destroy;
begin
  Buttons.Free;
  inherited Destroy;
end;

procedure TCDHeader.PaintExecute(Sender: TObject);
const
  Corner: Integer = 2;
  Border: Integer = 1;
  BorderColor: TColor = $B9C3C6;
  TopColor: TColor = $CFD6D9;
  BottomColor: TColor = $DAE0E1;
var
  Points: array of TPoint;
  TitleLeft: Integer;
  TitleWidth: Integer;
  TitleMaxWidth: Integer;
  I: Integer;
  Title: string;
  R: TRect;
begin
  if Assigned(Control) and Assigned(TWinControl(Control).DockManager) then
  with TCDManager(TWinControl(Control).DockManager) do
  case HeaderPos of
    hpLeft: begin
      Align := alLeft;
      Width := GrabberSize;
    end;
    hpTop, hpAuto: begin
      Align := alTop;
      Height := GrabberSize;
    end;
    hpRight: begin
      Align := alRight;
      Width := GrabberSize;
    end;
    hpBottom: begin
      Align := alBottom;
      Height := GrabberSize;
    end;
  end;

  if Assigned(Control) then
    if (Control as TWinControl).Focused then
      Canvas.Font.Style := Canvas.Font.Style + [fsBold]
        else Canvas.Font.Style := Canvas.Font.Style - [fsBold];

  RearrangeButtons;

  with Canvas do begin
    GradientFill(Rect(Border, Border, Width - Border,
      Height - Border), TopColor, BottomColor, gdVertical);
    Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    Pen.Color := clBtnFace;
    Pen.Style := psSolid;
    SetLength(Points, 3);
    Points[0] := Point(Border, Border);
    Points[1] := Point(Border, Border + Corner);
    Points[2] := Point(Border + Corner, Border);
    Polygon(Points);
    Points[0] := Point(Width - 1 - Border, Border);
    Points[1] := Point(Width - 1 - Border, Border + Corner);
    Points[2] := Point(Width - 1 - Border - Corner, Border);
    Polygon(Points);
    Points[0] := Point(Border, Height - 1 - Border);
    Points[1] := Point(Border, Height - 1 - Border - Corner);
    Points[2] := Point(Border + Corner, Height - 1 - Border);
    Polygon(Points);
    Points[0] := Point(Width - 1 - Border, Height - 1 - Border);
    Points[1] := Point(Width - 1 - Border, Height - 1 - Border - Corner);
    Points[2] := Point(Width - 1 - Border - Corner, Height - 1 - Border);
    Polygon(Points);

    SetLength(Points, 9);
    Points[0] := Point(Border, Border + Corner);
    Points[1] := Point(Border + Corner, Border);
    Points[2] := Point(Width - 1 - Border - Corner, Border);
    Points[3] := Point(Width - 1 - Border, Border + Corner);
    Points[4] := Point(Width - 1 - Border, Height - 1 - Border - Corner);
    Points[5] := Point(Width - 1 - Border - Corner, Height - 1 - Border);
    Points[6] := Point(Border + Corner, Height - 1 - Border);
    Points[7] := Point(Border, Height - 1 - Border - Corner);
    Points[8] := Point(Border, Border + Corner);
    Pen.Color := BorderColor;
    Polyline(Points);

    Canvas.Brush.Style := bsClear;
    TitleMaxWidth := Self.Width - 6;
    for I := 0 to Buttons.Count - 1 do
      if TCDHeaderButton(Buttons[I]).Visible then
        Dec(TitleMaxWidth, TCDHeaderButton(Buttons[I]).Icon.Width + 2);
    if Icon.Picture.Width > 0 then begin
      TitleLeft := 8 + Icon.Picture.Width;
      Dec(TitleMaxWidth, Icon.Picture.Width + 2)
    end else TitleLeft := 6;

    //SelectObject(Canvas.Handle, MyFont);
    if Assigned(Control) then
      Title := Control.Caption else Title := '';
    if (TextWidth(Title) > TitleMaxWidth) then begin
      while (Length(Title) > 0) and (TextWidth(Title + '...') > TitleMaxWidth) do begin
        Delete(Title, Length(Title), 1);
      end;
      Title := Title + '...';
    end;

    R := Rect(TitleLeft, 4, TitleLeft + TitleMaxWidth, 4 + TextHeight(Title));
    TextRect(R, TitleLeft, 4, Title);
  end;
end;

procedure TCDHeader.RearrangeButtons;
const
  Separation: Integer = 4;
var
  LeftPos: Integer;
  I: Integer;
begin
  LeftPos := Self.Width;
  for I := 0 to Buttons.Count - 1 do
  with TCDHeaderButton(Buttons[I]) do
  if Visible then begin
    Icon.Anchors := [akRight, akTop];
    //Icon.Picture.Bitmap.SetSize(16, 16);
    Icon.Width := Icon.Picture.Bitmap.Width;
    Icon.Height := Icon.Picture.Bitmap.Height;
    LeftPos := LeftPos - Icon.Width - Separation;
    Icon.Left := LeftPos;
    Icon.Top := (GrabberSize - Icon.Height) div 2;

    //ShowMessage(IntToStr(Icon.Width) + ' ' +  InttoStr(Icon.Height));
    Icon.Visible := True;
  end else Icon.Visible := False;
end;

procedure TCDHeader.CloseButtonClick(Sender: TObject);
begin
  if Assigned(Control) then
    Control.Hide;
end;

end.

