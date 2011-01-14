unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UDynNumber;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    DynamicNumber: TDynamicNumber;
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
var
  I: Integer;
  J: Integer;
  C: Integer;
  Parts: array of Integer;
  N: TDynamicNumber;
  Line: string;
begin
  Memo1.Clear;
  try
    N := TDynamicNumber.Create;
    for I := 0 to 16 do begin
      N.Stream.Size := 0;
      N.Write(I);
      Line := '';
      N.Stream.Position := 0;
      for J := 0 to N.Stream.Size - 1 do
        Line := Line + IntToStr(Integer(N.Stream.ReadBit));
      Memo1.Lines.Add(Line);
    end;
  finally
    N.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DynamicNumber := TDynamicNumber.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DynamicNumber.Free;
end;

end.

