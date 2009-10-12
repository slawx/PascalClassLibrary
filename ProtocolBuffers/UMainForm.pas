unit UMainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, UProtocolBuffers, UMemoryStreamEx;

const
  SampleProtoFileName = 'Sample.proto';

type
  { TMainForm }
  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    TreeView1: TTreeView;
    TreeView2: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure DisplayStream(Stream: TStream);
    procedure DisplayTree(ProtocolBuffer: TProtocolBuffer; TreeView: TTreeView);
    procedure MessageToNode(PBMessage: TPBMessageItem; Node: TTreeNode);
  public
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
  PB: TProtocolBuffer;
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
  PB: TProtocolBuffer;
begin
  Stream := TMemoryStreamEx.Create;
  PB := TProtocolBuffer.Create;
  with PB do begin
    with BaseMessage do begin
      Name := 'SampleMessage';
      NewItem := TPBIntegerItem.Create;
      TPBIntegerItem(NewItem).Name := 'Height';
      TPBIntegerItem(NewItem).Tag := 1;
      TPBIntegerItem(NewItem).Value := 12;
      Items.Add(NewItem);
      NewItem := TPBIntegerItem.Create;
      TPBIntegerItem(NewItem).Name := 'Width';
      TPBIntegerItem(NewItem).Tag := 2;
      TPBIntegerItem(NewItem).Value := 34;
      Items.Add(NewItem);
      NewItem := TPBIntegerItem.Create;
      TPBIntegerItem(NewItem).Name := 'Age';
      TPBIntegerItem(NewItem).Tag := 3;
      TPBIntegerItem(NewItem).Value := 45;
      Items.Add(NewItem);
    end;
    DisplayTree(PB, TreeView1);
    SaveToStream(Stream);
    Free;
  end;

  DisplayStream(Stream);
  Stream.Position := 0;

  PB := TProtocolBuffer.Create;
  with PB do begin
    with BaseMessage do begin
      Name := 'SampleMessage';
      NewItem := TPBIntegerItem.Create;
      TPBIntegerItem(NewItem).Name := 'Height';
      TPBIntegerItem(NewItem).Tag := 1;
      Items.Add(NewItem);
      NewItem := TPBIntegerItem.Create;
      TPBIntegerItem(NewItem).Name := 'Age';
      TPBIntegerItem(NewItem).Tag := 3;
      Items.Add(NewItem);
      NewItem := TPBIntegerItem.Create;
      TPBIntegerItem(NewItem).Name := 'Weight';
      TPBIntegerItem(NewItem).Tag := 4;
      Items.Add(NewItem);
    end;
    LoadFromStream(Stream);
    DisplayTree(PB, TreeView2);
    Free;
  end;
  Stream.Free;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
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

procedure TMainForm.DisplayTree(ProtocolBuffer: TProtocolBuffer;
  TreeView: TTreeView);
begin
  with TreeView do begin
    BeginUpdate;
    Items.Clear;
    TopItem := Items.Add(nil, 'message');
    MessageToNode(ProtocolBuffer.BaseMessage, TopItem);
    TopItem.Expand(True);
    EndUpdate;
  end;
end;

procedure TMainForm.MessageToNode(PBMessage: TPBMessageItem; Node: TTreeNode);
var
  I: Integer;
  NewNode: TTreeNode;
begin
  for I := 0 to PBMessage.Items.Count - 1 do begin
    NewNode := Node.Owner.AddChild(Node, '');
    if TPBItem(PBMessage.Items[I]) is TPBStringItem then begin
      NewNode.Text := IntToStr(TPBItem(PBMessage.Items[I]).Tag) + ': string ' +
        TPBItem(PBMessage.Items[I]).Name + ' = ' +
        TPBStringItem(PBMessage.Items[I]).Value;
    end else
    if TPBItem(PBMessage.Items[I]) is TPBIntegerItem then begin
      NewNode.Text := IntToStr(TPBItem(PBMessage.Items[I]).Tag) + ': uint32 ' +
        TPBItem(PBMessage.Items[I]).Name + ' = ' +
        IntToStr(TPBIntegerItem(PBMessage.Items[I]).Value);
    end else
    if TPBItem(PBMessage.Items[I]) is TPBMessageItem then begin
      NewNode.Text := IntToStr(TPBItem(PBMessage.Items[I]).Tag) + ': message ' +
        TPBItem(PBMessage.Items[I]).Name;
      MessageToNode(TPBMessageItem(PBMessage.Items[I]), NewNode);
    end;

  end;
end;

initialization
  {$I UMainForm.lrs}

end.

