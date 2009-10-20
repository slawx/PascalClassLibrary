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
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure DisplayStream(Stream: TStream);
    procedure DisplayTree(ProtocolBuffer: TProtocolBuffer; TreeView: TTreeView);
    procedure ListToNode(PBRepeated: TPBRepeatedItem; Node: TTreeNode;
      Definition: TPBDefinition);
    procedure MessageToNode(PBMessage: TPBMessageItem; Node: TTreeNode;
      Definition: TPBDefinition);
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

procedure TMainForm.Button3Click(Sender: TObject);
begin

end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  Stream: TMemoryStreamEx;
  NewItem: TPBDefinition;
  PB: TProtocolBuffer;
begin
  Stream := TMemoryStreamEx.Create;
  PB := TProtocolBuffer.Create;
  with PB do begin
    with Definition do begin
      Name := 'SampleMessage';
      NewItem := TPBDefinition.Create;
      with TPBDefinition(NewItem) do begin
        Name := 'Height';
        Tag := 1;
        ItemType := itInteger;
      end;
      Items.Add(NewItem);

      NewItem := TPBDefinition.Create;
      with TPBDefinition(NewItem) do begin
        Name := 'Name';
        Tag := 2;
        ItemType := itString;
        ItemMode := imOptional;
      end;
      Items.Add(NewItem);

      NewItem := TPBDefinition.Create;
      with TPBDefinition(NewItem) do begin
        Name := 'Age';
        Tag := 3;
        ItemType := itInteger;
        ItemMode := imOptional;
      end;
      Items.Add(NewItem);

      NewItem := TPBDefinition.Create;
      Items.Add(NewItem);
      with TPBDefinition(Items[Items.Count - 1]) do begin
        Name := 'Address';
        Tag := 5;
        ItemType := itMessage;
        NewItem := TPBDefinition.Create;
        ItemMode := imOptional;
        with TPBDefinition(NewItem) do begin
          Name := 'Street';
          Tag := 1;
          ItemType := itString;
          ItemMode := imOptional;
        end;
        Items.Add(NewItem);
      end;
    end;
    BaseMessage.Clear(Definition);
    TPBIntegerItem(PB.BaseMessage.Items[0]).Value := 12;
    TPBStringItem(TPBMessageItem(PB.BaseMessage.Items[3]).
      Items[0]).Value := 'Vsetínská';
    DisplayTree(PB, TreeView1);
    SaveToStream(Stream);
    Free;
  end;

  DisplayStream(Stream);
  Stream.Position := 0;

  PB := TProtocolBuffer.Create;
  with PB do begin
    with Definition do begin
      Name := 'SampleMessage';
      NewItem := TPBDefinition.Create;
      with TPBDefinition(NewItem) do begin
        Name := 'Height';
        Tag := 1;
        ItemType := itInteger;
        DefaultInteger := 32;
      end;
      Items.Add(NewItem);

      NewItem := TPBDefinition.Create;
      with TPBDefinition(NewItem) do begin
        Name := 'Name';
        Tag := 2;
        ItemType := itString;
        DefaultString := 'Billy Joe';
      end;
      Items.Add(NewItem);

      NewItem := TPBDefinition.Create;
      with TPBDefinition(NewItem) do begin
        Name := 'Work';
        Tag := 4;
        ItemType := itString;
        DefaultString := 'EasyCompany';
      end;
      Items.Add(NewItem);

      NewItem := TPBDefinition.Create;
      Items.Add(NewItem);
      with TPBDefinition(Items[Items.Count - 1]) do begin
        Name := 'Address';
        Tag := 5;
        ItemType := itMessage;
        NewItem := TPBDefinition.Create;
        with TPBDefinition(NewItem) do begin
          Name := 'Street';
          Tag := 1;
          ItemType := itString;
          DefaultString := 'Ruská';
        end;
        Items.Add(NewItem);
      end;
    end;
    BaseMessage.Clear(Definition);
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
    MessageToNode(ProtocolBuffer.BaseMessage, TopItem, ProtocolBuffer.Definition);
    TopItem.Expand(True);
    EndUpdate;
  end;
end;

procedure TMainForm.ListToNode(PBRepeated: TPBRepeatedItem; Node: TTreeNode;
  Definition: TPBDefinition);
var
  I: Integer;
  NewNode: TTreeNode;
begin
  for I := 0 to Definition.Items.Count - 1 do
  with TPBDefinition(Definition) do begin
    NewNode := Node.Owner.AddChild(Node, '');
    if ItemType = itString then begin
      NewNode.Text := IntToStr(Tag) + ': string ' + Name + ' = ' +
        TPBStringItem(PBRepeated.Items[I]).Value;
    end else
    if ItemType = itInteger then begin
      NewNode.Text := IntToStr(Tag) + ': uint32 ' + Name + ' = ' +
        IntToStr(TPBIntegerItem(PBRepeated.Items[I]).Value);
    end else
    if ItemType = itMessage then begin
      NewNode.Text := IntToStr(Tag) + ': message ' + Name;
      MessageToNode(TPBMessageItem(PBRepeated.Items[I]), NewNode, Definition);
    end;
  end;
end;

procedure TMainForm.MessageToNode(PBMessage: TPBMessageItem; Node: TTreeNode;
  Definition: TPBDefinition);
var
  I: Integer;
  NewNode: TTreeNode;
begin
  for I := 0 to Definition.Items.Count - 1 do
  with TPBDefinition(Definition.Items[I]) do begin
    NewNode := Node.Owner.AddChild(Node, '');
    if ItemMode = imRepeated then begin
      NewNode.Text := 'repeated';
      ListToNode(TPBRepeatedItem(PBMessage.Items[I]), NewNode, Definition.Items[I]);
    end else
    if ItemType = itString then begin
      NewNode.Text := IntToStr(Tag) + ': string ' + Name + ' = ' +
        TPBStringItem(PBMessage.Items[I]).Value;
    end else
    if ItemType = itInteger then begin
      NewNode.Text := IntToStr(Tag) + ': uint32 ' + Name + ' = ' +
        IntToStr(TPBIntegerItem(PBMessage.Items[I]).Value);
    end else
    if ItemType = itMessage then begin
      NewNode.Text := IntToStr(Tag) + ': message ' + Name;
      MessageToNode(TPBMessageItem(PBMessage.Items[I]), NewNode, Definition.Items[I]);
    end;
  end;
end;

initialization
  {$I UMainForm.lrs}

end.

