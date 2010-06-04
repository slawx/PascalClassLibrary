unit UPersistentForm;

{$mode delphi}

// Date: 2010-06-01

interface

uses
  Classes, SysUtils, Forms, URegistry, LCLIntf, Registry;

type

  { TPersistentForm }

  TPersistentForm = class
  public
    RegistryKey: string;
    RegistryRootKey: HKEY;
    procedure Load(Form: TForm);
    procedure Save(Form: TForm);
    constructor Create;
  end;

implementation

{ TPersistentForm }

procedure TPersistentForm.Load(Form: TForm);
var
  RestoredLeft, RestoredTop, RestoredWidth, RestoredHeight: Integer;
begin
  with TRegistryEx.Create do
    try
      RootKey := RegistryRootKey;
      OpenKey (RegistryKey + '\Forms\' + Form.Name, True);

      Form.Width := ReadIntegerWithDefault('Width', Form.Width);
      Form.Height := ReadIntegerWithDefault('Height', Form.Height);
      Form.Top := ReadIntegerWithDefault('Top', (Screen.Height - Form.Height) div 2);
      Form.Left := ReadIntegerWithDefault('Left', (Screen.Width - Form.Width) div 2);
      if Form.Left < 0 then
        Form.Left := 0;
      if Form.Left > (Screen.Width - 50) then
        Form.Left := Screen.Width - 50;
      if Form.Top < 0 then
        Form.Top := 0;
      if Form.Top > (Screen.Height - 50) then
        Form.Top := Screen.Height - 50;
      RestoredWidth := ReadIntegerWithDefault('RestoredWidth', Form.RestoredWidth);
      RestoredHeight := ReadIntegerWithDefault ('RestoredHeight', Form.RestoredHeight);
      RestoredTop := ReadIntegerWithDefault ('RestoredTop', (Screen.Height - Form.RestoredHeight) div 2);
      RestoredLeft := ReadIntegerWithDefault ('RestoredLeft', (Screen.Width - Form.RestoredWidth) div 2);
      Form.SetRestoredBounds(RestoredLeft, RestoredTop, RestoredWidth, RestoredHeight);

      Form.WindowState := TWindowState(ReadIntegerWithDefault('WindowState', Integer(wsNormal)));
      if ReadBoolWithDefault('Visible', False) then Form.Show;
    finally
      Free;
    end;
end;

procedure TPersistentForm.Save(Form: TForm);
begin
  with Form, TRegistryEx.Create do
    try
      RootKey := RegistryRootKey;
      OpenKey(RegistryKey + '\Forms\' + Form.Name, True);
      WriteInteger('Width', Form.Width);
      WriteInteger('Height', Form.Height);
      WriteInteger('Top', Form.Top);
      WriteInteger('Left', Form.Left);
      WriteInteger('RestoredWidth', Form.RestoredWidth);
      WriteInteger('RestoredHeight', Form.RestoredHeight);
      WriteInteger('RestoredTop', Form.RestoredTop);
      WriteInteger('RestoredLeft', Form.RestoredLeft);
      WriteInteger('WindowState', Integer(Form.WindowState));
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
