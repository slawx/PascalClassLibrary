unit UMainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, UProtocolBuffers, UMemoryStreamEx;

const
  SampleProtoFileName = 'Sample.proto';

type
  { TMainForm }
  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure DisplayStream(Stream: TStream);
  public
    PB: TProtocolBuffer;
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  Stream: TMemoryStream;
  StringList: TStringList;
begin
  Stream := TMemoryStreamEx.Create;
  StringList := TStringList.Create;
  StringList.LoadFromFile(SampleProtoFileName);
  PB := TProtocolBuffer.Create;
  with PB do begin
    LoadFromProto(StringList);
    SaveToStream(Stream);
  end;
  DisplayStream(Stream);
  StringList.Free;
  Stream.Free;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  Stream: TMemoryStreamEx;
  NewItem: TPBItem;
begin
  Stream := TMemoryStreamEx.Create;
  PB := TProtocolBuffer.Create;
  with PB do begin
    with BaseMessage do begin
      Name := 'SampleMessage';
      NewItem := TPBIntegerItem.Create;
      TPBIntegerItem(NewItem).Value := $5555555;
      Items.Add(NewItem);
    end;
    SaveToStream(Stream);
    DisplayStream(Stream);
  end;
  Stream.Free;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  PB.Free;
end;

procedure TMainForm.DisplayStream(Stream: TStream);
var
  I: Integer;
  Text: string;
begin
  Stream.Position := 0;
  Text := '';
  for I := 1 to Stream.Size do begin
    Text := Text + IntToHex(Stream.ReadByte, 2) + ' ';
  end;
  Memo1.Lines.Text := Text;
end;

initialization
  {$I UMainForm.lrs}

end.

