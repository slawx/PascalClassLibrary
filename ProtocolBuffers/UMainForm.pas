unit UMainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, UProtocolBuffers, UMemoryStreamEx;

type
  { TMainForm }
  TMainForm = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    PB: TProtocolBuffer;
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Stream: TMemoryStreamEx;
  Text: string;
  I: Integer;
begin
  Stream := TMemoryStreamEx.Create;
  PB := TProtocolBuffer.Create;
  with PB do begin
    BaseMessage := TPBMessage.Create;
    BaseMessage.Name := 'SampleMessage';
    BaseMessage.Tag := $aaaa;
    SaveToStream(Stream);
  end;

  Stream.Position := 0;
  Text := '';
  for I := 1 to Stream.Size do begin
    Text := Text + IntToHex(Stream.ReadByte, 2) + ' ';
  end;
  Memo1.Lines.Text := Text;
  Stream.Free;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  PB.Free;
end;

initialization
  {$I UMainForm.lrs}

end.

