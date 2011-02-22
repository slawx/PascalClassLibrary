unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, UVarBlockSerializer, StrUtils, ComCtrls, ExtCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
  published
    ButtonDecodeString: TButton;
    ButtonDecodeRaw: TButton;
    ButtonEncodeString: TButton;
    ButtonEncodeRaw: TButton;
    ButtonFloatDecode1: TButton;
    ButtonFloatEncode1: TButton;
    ButtonUIntDecode: TButton;
    ButtonSIntDecode: TButton;
    ButtonSIntEncode: TButton;
    EditRaw: TEdit;
    EditFloat: TEdit;
    EditStringData: TEdit;
    EditString: TEdit;
    EditRawData: TEdit;
    EditUInt: TEdit;
    ButtonUIntEncode: TButton;
    EditSInt: TEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    SpinEditFloat: TSpinEdit;
    SpinEditUInt: TSpinEdit;
    SpinEditSInt: TSpinEdit;
    procedure ButtonDecodeRawClick(Sender: TObject);
    procedure ButtonDecodeStringClick(Sender: TObject);
    procedure ButtonEncodeRawClick(Sender: TObject);
    procedure ButtonEncodeStringClick(Sender: TObject);
    procedure ButtonFloatDecode1Click(Sender: TObject);
    procedure ButtonFloatEncode1Click(Sender: TObject);
    procedure ButtonSIntDecodeClick(Sender: TObject);
    procedure ButtonSIntEncodeClick(Sender: TObject);
    procedure ButtonUIntDecodeClick(Sender: TObject);
    procedure ButtonUIntEncodeClick(Sender: TObject);
  public
    function StreamToString(Stream: TStream): string;
    procedure StringToStream(Text: string; Stream: TStream);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ButtonDecodeRawClick(Sender: TObject);
var
  Block: TVarBlockSerializer;
  Stream: TMemoryStream;
begin
  try
    Block := TVarBlockSerializer.Create;
    Stream := TMemoryStream.Create;
    StringToStream(EditRawData.Text, Block.Stream);
    Block.Stream.Position := 0;
    Block.ReadVarStream(Stream);
    EditRaw.Text := StreamToString(Stream);
  finally
    Stream.Free;
    Block.Free;
  end;
end;

procedure TMainForm.ButtonDecodeStringClick(Sender: TObject);
var
  Block: TVarBlockSerializer;
begin
  try
    Block := TVarBlockSerializer.Create;
    StringToStream(EditStringData.Text, Block.Stream);
    Block.Stream.Position := 0;
    EditString.Text := Block.ReadVarString;
  finally
    Block.Free;
  end;
end;

procedure TMainForm.ButtonEncodeRawClick(Sender: TObject);
var
  Block: TVarBlockSerializer;
  Stream: TMemoryStream;
begin
  try
    Block := TVarBlockSerializer.Create;
    Stream := TMemoryStream.Create;
    StringToStream(EditRaw.Text, Stream);
    Block.WriteVarStream(Stream);
    EditRawData.Text := StreamToString(Block.Stream);
  finally
    Stream.Free;
    Block.Free;
  end;
end;

procedure TMainForm.ButtonEncodeStringClick(Sender: TObject);
var
  Block: TVarBlockSerializer;
begin
  try
    Block := TVarBlockSerializer.Create;
    Block.WriteVarString(EditString.Text);
    EditStringData.Text := StreamToString(Block.Stream);
  finally
    Block.Free;
  end;
end;

procedure TMainForm.ButtonFloatDecode1Click(Sender: TObject);
var
  Block: TVarBlockSerializer;
begin
  try
    Block := TVarBlockSerializer.Create;
    StringToStream(EditFloat.Text, Block.Stream);
    Block.Stream.Position := 0;
    FloatSpinEdit1.Value := Block.ReadVarFloat(SpinEditFloat.Value);
  finally
    Block.Free;
  end;
end;

procedure TMainForm.ButtonFloatEncode1Click(Sender: TObject);
var
  Block: TVarBlockSerializer;
begin
  try
    Block := TVarBlockSerializer.Create;
    Block.WriteVarFloat(FloatSpinEdit1.Value, SpinEditFloat.Value);
    EditFloat.Text := StreamToString(Block.Stream);
  finally
    Block.Free;
  end;
end;

procedure TMainForm.ButtonSIntDecodeClick(Sender: TObject);
var
  Block: TVarBlockSerializer;
begin
  try
    Block := TVarBlockSerializer.Create;
    StringToStream(EditSInt.Text, Block.Stream);
    Block.Stream.Position := 0;
    SpinEditSInt.Value := Block.ReadVarSInt;
  finally
    Block.Free;
  end;
end;

procedure TMainForm.ButtonSIntEncodeClick(Sender: TObject);
var
  Block: TVarBlockSerializer;
begin
  try
    Block := TVarBlockSerializer.Create;
    Block.WriteVarSInt(SpinEditSInt.Value);
    EditSInt.Text := StreamToString(Block.Stream);
  finally
    Block.Free;
  end;
end;

procedure TMainForm.ButtonUIntDecodeClick(Sender: TObject);
var
  Block: TVarBlockSerializer;
begin
  try
    Block := TVarBlockSerializer.Create;
    StringToStream(EditUInt.Text, Block.Stream);
    Block.Stream.Position := 0;
    SpinEditUInt.Value := Block.ReadVarUInt;
  finally
    Block.Free;
  end;
end;

procedure TMainForm.ButtonUIntEncodeClick(Sender: TObject);
var
  Block: TVarBlockSerializer;
begin
  try
    Block := TVarBlockSerializer.Create;
    Block.WriteVarUInt(SpinEditUInt.Value);
    EditUInt.Text := StreamToString(Block.Stream);
  finally
    Block.Free;
  end;
end;

function TMainForm.StreamToString(Stream: TStream): string;
begin
  Result := '';
  Stream.Position := 0;
  while Stream.Position < Stream.Size do
    Result := Result + IntToHex(Stream.ReadByte, 2) + ' ';
end;

function TryHexToInt(Data: string; var Value: Integer): Boolean;
var
  I: Integer;
begin
  Data := UpperCase(Data);
  Result := True;
  Value := 0;
  for I := 0 to Length(Data) - 1 do begin
    if (Data[I + 1] >= '0') and (Data[I + 1] <= '9') then
      Value := Value or (Ord(Data[I + 1]) - Ord('0')) shl ((Length(Data) - I - 1) * 4)
    else if (Data[I + 1] >= 'A') and (Data[I + 1] <= 'F') then
      Value := Value or (Ord(Data[I + 1]) - Ord('A') + 10) shl ((Length(Data) - I - 1) * 4)
    else Result := False;
  end;
end;

procedure TMainForm.StringToStream(Text: string; Stream: TStream);
var
  Part: string;
  Number: Integer;
begin
  Stream.Size := 0;
  Text := Trim(Text);
  while Pos(' ', Text) > 0 do begin
    Part := Copy(Text, 1, Pos(' ', Text) - 1);
    if TryHexToInt(Part, Number) and (Number < 256) then
      Stream.WriteByte(Number);
    Delete(Text, 1, Pos(' ', Text));
  end;
  if TryHexToInt(Text, Number) and (Number < 256) then
    Stream.WriteByte(Number);
end;

end.

