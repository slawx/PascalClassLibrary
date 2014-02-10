unit UPersistentForm;

{$mode delphi}

// Date: 2010-06-01

interface

uses
  Classes, SysUtils, Forms, URegistry, LCLIntf, Registry;

type

  { TPersistentForm }

  TPersistentForm = class(TComponent)
  private
    FEntireVisible: Boolean;
    FMinVisiblePart: Integer;
    FRegistryContext: TRegistryContext;
  public
    function CheckEntireVisible(Rect: TRect): TRect;
    function CheckPartVisible(Rect: TRect; Part: Integer): TRect;
    procedure Load(Form: TForm; DefaultMaximized: Boolean = False);
    procedure Save(Form: TForm);
    constructor Create(AOwner: TComponent); override;
    property RegistryContext: TRegistryContext read FRegistryContext
      write FRegistryContext;
  published
    property MinVisiblePart: Integer read FMinVisiblePart write FMinVisiblePart;
    property EntireVisible: Boolean read FEntireVisible write FEntireVisible;
  end;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('Common', [TPersistentForm]);
end;

{ TPersistentForm }

function TPersistentForm.CheckEntireVisible(Rect: TRect): TRect;
var
  Width: Integer;
  Height: Integer;
begin
  Result := Rect;
  Width := Rect.Right - Rect.Left;
  Height := Rect.Bottom - Rect.Top;
  if Result.Left < (Screen.DesktopLeft) then begin
    Result.Left := Screen.DesktopLeft;
    Result.Right := Screen.DesktopLeft + Width;
  end;
  if Result.Right > (Screen.DesktopLeft + Screen.DesktopWidth) then begin
    Result.Left := Screen.DesktopLeft + Screen.DesktopWidth - Width;
    Result.Right := Screen.DesktopLeft + Screen.DesktopWidth;
  end;
  if Result.Top < Screen.DesktopTop then begin
    Result.Top := Screen.DesktopTop;
    Result.Bottom := Screen.DesktopTop + Height;
  end;
  if Result.Bottom > (Screen.DesktopTop + Screen.DesktopHeight) then begin
    Result.Top := Screen.DesktopTop + Screen.DesktopHeight - Height;
    Result.Bottom := Screen.DesktopTop + Screen.DesktopHeight;
  end;
end;

function TPersistentForm.CheckPartVisible(Rect: TRect; Part: Integer): TRect;
var
  Width: Integer;
  Height: Integer;
begin
  Result := Rect;
  Width := Rect.Right - Rect.Left;
  Height := Rect.Bottom - Rect.Top;
  if Result.Right < (Screen.DesktopLeft + Part) then begin
    Result.Left := Screen.DesktopLeft + Part - Width;
    Result.Right := Screen.DesktopLeft + Part;
  end;
  if Result.Left > (Screen.DesktopLeft + Screen.DesktopWidth - Part) then begin
    Result.Left := Screen.DesktopLeft + Screen.DesktopWidth - Part;
    Result.Right := Screen.DesktopLeft + Screen.DesktopWidth - Part + Width;
  end;
  if Result.Bottom < (Screen.DesktopTop + Part) then begin
    Result.Top := Screen.DesktopTop + Part - Height;
    Result.Bottom := Screen.DesktopTop + Part;
  end;
  if Result.Top > (Screen.DesktopTop + Screen.DesktopHeight - Part) then begin
    Result.Top := Screen.DesktopTop + Screen.DesktopHeight - Part;
    Result.Bottom := Screen.DesktopTop + Screen.DesktopHeight - Part + Height;
  end;
end;

procedure TPersistentForm.Load(Form: TForm; DefaultMaximized: Boolean = False);
var
  Normal: TRect;
  Restored: TRect;
  LoadDefaults: Boolean;
begin
  with TRegistryEx.Create do
    try
      RootKey := RegistryContext.RootKey;
      OpenKey(RegistryContext.Key + '\Forms\' + Form.Name, True);

      //RestoredWindowState := TWindowState(ReadIntegerWithDefault('WindowState', Integer(Form.WindowState)));
      //if RestoredWindowState = wsMinimized then
      //  RestoredWindowState := wsNormal;
      //Form.WindowState := RestoredWindowState;
      LoadDefaults := not ValueExists('NormalLeft');
      Normal := Bounds(ReadIntegerWithDefault('NormalLeft', (Screen.Width - Form.Width) div 2),
        ReadIntegerWithDefault('NormalTop', (Screen.Height - Form.Height) div 2),
        ReadIntegerWithDefault('NormalWidth', Form.Width),
        ReadIntegerWithDefault('NormalHeight', Form.Height));
      Restored := Bounds(ReadIntegerWithDefault('RestoredLeft', (Screen.Width - Form.Width) div 2),
        ReadIntegerWithDefault('RestoredTop', (Screen.Height - Form.Height) div 2),
        ReadIntegerWithDefault('RestoredWidth', Form.Width),
        ReadIntegerWithDefault('RestoredHeight', Form.Height));

      if not EqualRect(Normal, Restored) or
        (LoadDefaults and DefaultMaximized) then begin
        // Restore to maximized state
        Form.WindowState := wsNormal;
        if not EqualRect(Restored, Form.BoundsRect) then
          Form.BoundsRect := Restored;
        Form.WindowState := wsMaximized;
      end else begin
        // Restore to normal state
        Form.WindowState := wsNormal;
        if FEntireVisible then Normal := CheckEntireVisible(Normal)
          else if FMinVisiblePart > 0 then
        Normal := CheckPartVisible(Normal, FMinVisiblePart);
        if not EqualRect(Normal, Form.BoundsRect) then
          Form.BoundsRect := Normal;
      end;

      //if ReadBoolWithDefault('Visible', False) then Form.Show;
    finally
      Free;
    end;
end;

procedure TPersistentForm.Save(Form: TForm);
begin
  with Form, TRegistryEx.Create do
    try
      RootKey := RegistryContext.RootKey;
      OpenKey(RegistryContext.Key + '\Forms\' + Form.Name, True);
      WriteInteger('NormalWidth', Form.Width);
      WriteInteger('NormalHeight', Form.Height);
      WriteInteger('NormalTop', Form.Top);
      WriteInteger('NormalLeft', Form.Left);
      WriteInteger('RestoredWidth', Form.RestoredWidth);
      WriteInteger('RestoredHeight', Form.RestoredHeight);
      WriteInteger('RestoredTop', Form.RestoredTop);
      WriteInteger('RestoredLeft', Form.RestoredLeft);
      //WriteInteger('WindowState', Integer(Form.WindowState));
      //WriteBool('Visible', Form.Visible);
    finally
      Free;
    end;
end;

constructor TPersistentForm.Create(AOwner: TComponent);
begin
  inherited;
  FMinVisiblePart := 50;
  FRegistryContext.RootKey := HKEY_CURRENT_USER;
end;

end.
