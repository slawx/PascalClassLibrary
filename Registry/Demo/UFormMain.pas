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
    AImport: TAction;
    AExport: TAction;
    ActionList1: TActionList;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    TreeView1: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
  private
    function RegValueToString(Name: string): string;
    procedure LoadNode(Node: TTreeNode; Key: TRegKey);
  public
    procedure ReloadValues;
    procedure ReloadKeys;
  end;

var
  FormMain: TFormMain;

implementation

uses
  UCore;

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

procedure TFormMain.FormShow(Sender: TObject);
begin
  ReloadKeys;
  ReloadValues;
end;

function TFormMain.RegValueToString(Name: string): string;
var
  ValueType: TRegValueType;
  Buffer: array of Byte;
  I: Integer;
begin
  with Core.ActiveRegistry do begin
    ValueType := GetValueType(Name);
    case ValueType of
      vtBoolean: if ReadBool(Name) then Result := 'True' else Result := 'False';
      vtInteger: Result := IntToStr(ReadInteger(Name));
      vtString: Result := ReadString(Name);
      vtFloat: Result := FloatToStr(ReadFloat(Name));
      vtUnknown: Result := '?';
      vtText: Result := ReadString(Name);
      vtBinary: begin
        SetLength(Buffer, GetValueSize(Name));
        ReadBinaryData(Name, PByte(Buffer)^, Length(Buffer));
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

procedure TFormMain.ReloadKeys;
var
  NewNode: TTreeNode;
begin
  NewNode := TreeView1.Items.AddChild(nil, 'Local computer');
end;

end.

