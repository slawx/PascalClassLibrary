unit UFormMain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, ExtCtrls, UGeneralRegistry;

type

  { TFormMain }

  TFormMain = class(TForm)
    AConnectionAdd: TAction;
    AConnectionDelete: TAction;
    AConnectionModify: TAction;
    AValueAdd: TAction;
    AValueEdit: TAction;
    AValueDelete: TAction;
    AImport: TAction;
    AExport: TAction;
    ActionList1: TActionList;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PopupMenuValue: TPopupMenu;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    TreeView1: TTreeView;
    procedure AConnectionAddExecute(Sender: TObject);
    procedure AValueAddExecute(Sender: TObject);
    procedure AValueDeleteExecute(Sender: TObject);
    procedure AValueEditExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
  private
    function RegValueToString(ValueName: string): string;
    procedure LoadNode(Node: TTreeNode; Key: TRegKey);
    procedure ReloadTreeNode(Node: TTreeNode; Reg: TGeneralRegistry);
  public
    procedure ReloadValues;
    procedure ReloadKeys;
  end;

var
  FormMain: TFormMain;


implementation

uses
  UCore, UFormValue;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ListView1Data(Sender: TObject; Item: TListItem);
var
  KeyInfo: TRegKeyInfo;
  ValueInfo: TRegValueInfo;
  ValueNames: TStringList;
begin
  try
    ValueNames := TStringList.Create;
  Core.ActiveRegistry.GetKeyInfo(KeyInfo);
  if (Item.Index >= 0) and (Item.Index < KeyInfo.NumberValues) then begin
    Core.ActiveRegistry.GetValueNames(ValueNames);
    Item.Caption := ValueNames[Item.Index];
    Core.ActiveRegistry.GetValueInfo(ValueNames[Item.Index], ValueInfo);
    Item.SubItems.Add(RegValueTypeName[ValueInfo.ValueType]);
    Item.SubItems.Add(RegValueToString(ValueNames[Item.Index]));
  end;
  finally
    ValueNames.Free;
  end;
end;

procedure TFormMain.ListView1DblClick(Sender: TObject);
begin
  AValueEdit.Execute;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  ReloadKeys;
  ReloadValues;
end;


procedure TFormMain.AConnectionAddExecute(Sender: TObject);
begin
end;

procedure TFormMain.AValueDeleteExecute(Sender: TObject);
var
  I: Integer;
begin
  if MessageDlg('Delete value', 'Do you want to delete value?', mtConfirmation,
    [mbCancel, mbOk], 0) = mrOk then begin
      for I := ListView1.Items.Count - 1 downto 0 do
      if ListView1.Items[I].Selected then begin
        ListView1.Items[I].Selected := False;
        Core.ActiveRegistry.DeleteValue(ListView1.Items[I].Caption);
      end;
      ReloadValues;
    end;
end;

procedure TFormMain.AValueAddExecute(Sender: TObject);
var
  FormValue: TFormValue;
begin
  FormValue := TFormValue.Create(nil);
  FormValue.EditName.Text := 'New value';
  FormValue.EditName.Enabled := True;
  FormValue.SetValueType(vtString);
  if FormValue.ShowModal = mrOk then begin
    if FormValue.GetValueType = vtString then
      Core.ActiveRegistry.WriteString(FormValue.EditName.Text, FormValue.EditValue.Text)
    else if FormValue.GetValueType = vtInteger then
      Core.ActiveRegistry.WriteInteger(FormValue.EditName.Text, FormValue.SpinEditValue.Value)
    else if FormValue.GetValueType = vtBoolean then
      Core.ActiveRegistry.WriteBool(FormValue.EditName.Text, FormValue.CheckBoxValue.Checked)
    else raise Exception.Create('Unsupported type ' + RegValueTypeName[TRegValueType(FormValue.ComboBoxType.ItemIndex)]);
    ReloadValues;
  end;
  FormValue.Free;
end;

procedure TFormMain.AValueEditExecute(Sender: TObject);
var
  FormValue: TFormValue;
  ValueInfo: TRegValueInfo;
  PrevValueType: TRegValueType;
begin
  FormValue := TFormValue.Create(nil);
  FormValue.EditName.Text := ListView1.Selected.Caption;
  FormValue.EditName.Enabled := False;
  Core.ActiveRegistry.GetValueInfo(ListView1.Selected.Caption, ValueInfo);
  PrevValueType := ValueInfo.ValueType;
  FormValue.SetValueType(ValueInfo.ValueType);
  if PrevValueType = vtString then
    FormValue.EditValue.Text := Core.ActiveRegistry.ReadString(ListView1.Selected.Caption)
  else if PrevValueType = vtInteger then
    FormValue.SpinEditValue.Value := Core.ActiveRegistry.ReadInteger(ListView1.Selected.Caption)
  else if PrevValueType = vtBoolean then
    FormValue.CheckBoxValue.Checked := Core.ActiveRegistry.ReadBool(ListView1.Selected.Caption);
  if FormValue.ShowModal = mrOk then begin
    if FormValue.GetValueType <> PrevValueType then
      Core.ActiveRegistry.DeleteValue(FormValue.EditName.Text);
    if FormValue.GetValueType = vtString then
      Core.ActiveRegistry.WriteString(FormValue.EditName.Text, FormValue.EditValue.Text)
    else if FormValue.GetValueType = vtInteger then
      Core.ActiveRegistry.WriteInteger(FormValue.EditName.Text, FormValue.SpinEditValue.Value)
    else if FormValue.GetValueType = vtBoolean then
      Core.ActiveRegistry.WriteBool(FormValue.EditName.Text, FormValue.CheckBoxValue.Checked)
    else raise Exception.Create('Unsupported type ' + RegValueTypeName[TRegValueType(FormValue.ComboBoxType.ItemIndex)]);
    ReloadValues;
  end;
  FormValue.Free;
end;

function TFormMain.RegValueToString(ValueName: string): string;
var
  ValueType: TRegValueType;
  Buffer: array of Byte;
  I: Integer;
begin
  with Core.ActiveRegistry do begin
    ValueType := GetValueType(ValueName);
    case ValueType of
      vtBoolean: if ReadBool(ValueName) then Result := 'True' else Result := 'False';
      vtInteger: Result := IntToStr(ReadInteger(ValueName));
      vtString: Result := ReadString(ValueName);
      vtFloat: Result := FloatToStr(ReadFloat(ValueName));
      vtUnknown: Result := '?';
      vtText: Result := ReadString(ValueName);
      vtBinary: begin
        SetLength(Buffer, GetValueSize(ValueName));
        ReadBinaryData(ValueName, PByte(Buffer)^, Length(Buffer));
        Result := '';
        for I := 0 to Length(Buffer) - 1 do begin
          Result := Result + IntToHex(Buffer[I], 2);
          if I < Length(Buffer) - 1 then Result := Result + ' ';
        end;
      end;
    end;
  end;
end;

procedure TFormMain.LoadNode(Node: TTreeNode; Key: TRegKey);
begin

end;

procedure TFormMain.ReloadValues;
var
  KeyInfo: TRegKeyInfo;
begin
  if Core.ActiveRegistry.GetKeyInfo(KeyInfo) then
    ListView1.Items.Count := KeyInfo.NumberValues
    else ListView1.Items.Count := 0;
  ListView1.Refresh;
end;

procedure TFormMain.ReloadTreeNode(Node: TTreeNode; Reg: TGeneralRegistry);
var
  Keys: TStrings;
  I: Integer;
  NewNode: TTreeNode;
begin
  Keys := TStringList.Create;
  Reg.GetKeyNames(Keys);
  for I := 0 to Keys.Count - 1 do begin
    NewNode := Node.TreeNodes.AddChild(Node, Keys[I]);
    Reg.OpenKey(Keys[I], False);
    ReloadTreeNode(NewNode, Reg);
  end;
  Keys.Free;
end;

procedure TFormMain.ReloadKeys;
var
  NewNode: TTreeNode;
begin
  TreeView1.Items.Clear;
  NewNode := TreeView1.Items.AddChild(nil, 'Local computer');
  ReloadTreeNode(NewNode, Core.ActiveRegistry);
  NewNode.Expand(True);
end;

end.

