unit UPersistentForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, URegistry, Windows;

type

  { TPersistentForm }

  TPersistentForm = class
  public
    RegistryKey: string;
    RegistryRootKey: Cardinal;
    procedure Load(Form: TForm);
    procedure Save(Form: TForm);
    constructor Create;
  end;

implementation

{ TPersistentForm }

procedure TPersistentForm.Load(Form: TForm);
var
  Pl : TWindowPlacement;  // used for API call
  R : TRect;               // used for wdw pos
  Width, Height, Top, Left : Integer;
begin
  with TRegistryEx.Create do
    try
      RootKey := RegistryRootKey;
      OpenKey (RegistryKey + '\Forms\' + Form.Name, True);

      Width := ReadIntegerWithDefault ('Width', Form.Width);
      Height := ReadIntegerWithDefault ('Height', Form.Height);
      Top := ReadIntegerWithDefault ('Top', (Screen.Height - Form.Height) div 2);
      Left := ReadIntegerWithDefault ('Left', (Screen.Width - Form.Width) div 2);
      if Left < 0 then
        Left := 0;
      if Left > (Screen.Width - 50) then
        Left := Screen.Width - 50;
      if Top < 0 then
        Top := 0;
      if Top > (Screen.Height - 50) then
        Top := Screen.Height - 50;

      // Restore position using WinAPI
      Pl.Length := SizeOf (TWindowPlacement);
      GetWindowPlacement (Form.Handle, @Pl);
      R := Pl.rcNormalPosition;
      R.Left := Left;
      R.Top := Top;
      R.Right := Left + Width;
      R.Bottom := Top + Height;
      Pl.rcNormalPosition := R;
      if ReadBoolWithDefault('Maximized', False) then
        Pl.showCmd := SW_SHOWMAXIMIZED else Pl.showCmd := SW_HIDE;
      SetWindowPlacement(Form.Handle, @Pl);

      if ReadBoolWithDefault('Visible', False) then Form.Show;
    finally
      Free;
    end;
end;

procedure TPersistentForm.Save(Form: TForm);
var
  Pl: TWindowPlacement;  // used for API call
  R: TRect;               // used for wdw pos
begin
  {Calculate window's normal size and position using
  Windows API call - the form's Width, Height, Top and
  Left properties will give maximized window size if
  form is maximised, which is not what we want here}
  Pl.Length := SizeOf (TWindowPlacement);
  GetWindowPlacement (Form.Handle, @Pl);
  R := Pl.rcNormalPosition;

  with Form, TRegistryEx.Create do
    try
      RootKey := RegistryRootKey;
      OpenKey(RegistryKey + '\Forms\' + Form.Name, True);
      WriteInteger('Width', R.Right - R.Left);
      WriteInteger('Height', R.Bottom - R.Top);
      WriteInteger('Top', R.Top);
      WriteInteger('Left', R.Left);
      WriteBool('Maximized', WindowState = wsMaximized);
      WriteBool('Visible', Form.Visible);
    finally
      Free;
    end;
end;

constructor TPersistentForm.Create;
begin
  RegistryRootKey := HKEY_CURRENT_USER;
end;

end.

