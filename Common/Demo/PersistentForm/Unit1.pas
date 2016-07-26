unit Unit1;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, XMLConf, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ExtCtrls, ComCtrls, Menus, XMLPropStorage, UPersistentForm;

type

  { TForm1 }

  TForm1 = class(TForm)
    ListView1: TListView;
    PersistentForm1: TPersistentForm;
    Timer1: TTimer;
    XMLConfig1: TXMLConfig;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure AddValue(Name, Value: string);
  public
    { public declarations }
    procedure ShowDimensions;
  end;

var
  Form1: TForm1;

const
  WindowStateText: array[TWindowState] of string = ('wsNormal', 'wsMinimized',
    'wsMaximized', 'wsFullScreen');

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  PersistentForm1.Load(Self);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  ShowDimensions;
end;

procedure TForm1.AddValue(Name, Value: string);
var
  NewItem: TListItem;
begin
  NewItem := ListView1.Items.Add;
  NewItem.Caption := Name;
  NewItem.SubItems.Add(Value);
end;

procedure TForm1.ShowDimensions;
begin
  with ListView1.Items do begin
    Clear;
    AddValue('Form.Left', IntToStr(Self.Left));
    AddValue('Form.Top', IntToStr(Self.Top));
    AddValue('Form.Width', IntToStr(Self.Width));
    AddValue('Form.Height', IntToStr(Self.Height));
    AddValue('Form.RestoredLeft', IntToStr(Self.RestoredLeft));
    AddValue('Form.RestoredTop', IntToStr(Self.RestoredTop));
    AddValue('Form.RestoredWidth', IntToStr(Self.RestoredWidth));
    AddValue('Form.RestoredHeight', IntToStr(Self.RestoredHeight));
    AddValue('Form.BoundsRect.Left', IntToStr(Self.BoundsRect.Left));
    AddValue('Form.BoundsRect.Top', IntToStr(Self.BoundsRect.Top));
    AddValue('Form.BoundsRect.Right', IntToStr(Self.BoundsRect.Right));
    AddValue('Form.BoundsRect.Bottom', IntToStr(Self.BoundsRect.Bottom));
    AddValue('Screen.DesktopLeft', IntToStr(Screen.DesktopLeft));
    AddValue('Screen.DesktopTop', IntToStr(Screen.DesktopTop));
    AddValue('Screen.DesktopWidth', IntToStr(Screen.DesktopWidth));
    AddValue('Screen.DesktopHeight', IntToStr(Screen.DesktopHeight));
    AddValue('WindowState', WindowStateText[Self.WindowState]);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  PersistentForm1.Save(Self);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  ShowDimensions;
end;

end.

