unit UMainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, SpecializedList, SpecializedDictionary, SpecializedQueue,
  DateUtils, SpecializedMatrix, SpecializedStream;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonStreamByte: TButton;
    ButtonBenchmarkDictionary: TButton;
    ButtonBenchmarkListPointer: TButton;
    ButtonListObject: TButton;
    ButtonBenchmarkListString: TButton;
    ButtonCharList: TButton;
    ButtonMatrixInteger: TButton;
    ButtonQueueInteger: TButton;
    ButtonDictionaryString: TButton;
    ButtonIntegerList: TButton;
    ButtonStringList: TButton;
    Label1: TLabel;
    LabelTestName: TLabel;
    ListViewOutput: TListView;
    procedure ButtonBenchmarkDictionaryClick(Sender: TObject);
    procedure ButtonBenchmarkListPointerClick(Sender: TObject);
    procedure ButtonBenchmarkListStringClick(Sender: TObject);
    procedure ButtonCharListClick(Sender: TObject);
    procedure ButtonDictionaryStringClick(Sender: TObject);
    procedure ButtonIntegerListClick(Sender: TObject);
    procedure ButtonMatrixIntegerClick(Sender: TObject);
    procedure ButtonListObjectClick(Sender: TObject);
    procedure ButtonQueueIntegerClick(Sender: TObject);
    procedure ButtonStringListClick(Sender: TObject);
    procedure ButtonStreamByteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    MeasureDuration: TDateTime;
    Bitmap: TBitmap;
    procedure UpdateButtonState(Enabled: Boolean);
    procedure WriteOutput(Text1: string = ''; Text2: string = '');
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MeasureDuration := 100 * OneMillisecond;
end;

procedure TMainForm.ButtonIntegerListClick(Sender: TObject);
var
  List: TListInteger;
  List2: TListInteger;
  I: Integer;
begin
  ListViewOutput.Clear;
  LabelTestName.Caption := 'TListInteger test';
  List := TListInteger.Create;
  List2 := TListInteger.Create;
  with List do try
    AddArray([10, 20, 30, 40]);
    WriteOutput('AddArray([10, 20, 30, 40])', Implode(',', IntToStr));
    Clear;
    WriteOutput('Clear', Implode(',', IntToStr));
    for I := 0 to 10 do Add(I);
    WriteOutput('for I := 0 to 10 do Add(I)', Implode(',', IntToStr));
    WriteOutput('Count', IntToStr(Count));
    Reverse;
    WriteOutput('Reverse', Implode(',', IntToStr));
    WriteOutput('First', IntToStr(First));
    WriteOutput('Last', IntToStr(Last));
    MoveItems(3, 2, 3);
    WriteOutput('MoveItems(3, 2, 3)', Implode(',', IntToStr));
    Insert(5, 11);
    WriteOutput('Insert(5, 11)', Implode(',', IntToStr));
    DeleteItems(0, 10);
    WriteOutput('Delete(0, 10)', Implode(',', IntToStr));
    List2.SetArray([1, 0]);
    WriteOutput('EqualTo([6, 11])', BoolToStr(EqualTo(List2)));
    List2.SetArray([2, 0]);
    WriteOutput('EqualTo([7, 11])', BoolToStr(EqualTo(List2)));
    InsertCount(0, 3);
    WriteOutput('InsertCount(0, 3)', Implode(',', IntToStr));
    Fill(0, 3, 9);
    WriteOutput('Fill(0, 3, 9)', Implode(',', IntToStr));
  finally
    Free;
    List2.Free;
  end;
end;

procedure TMainForm.ButtonMatrixIntegerClick(Sender: TObject);
var
  Matrix: TMatrixInteger;
  I: Integer;
begin
  ListViewOutput.Clear;
  LabelTestName.Caption := 'TMatrixInteger test';
  Matrix := TMatrixInteger.Create;
  with Matrix do try
    Count := CreateIndex(2, 2);
    WriteOutput('Count := CreateIndex(2, 2)', '[' + Implode('; ', ', ', IntToStr) + ']');
    Fill(CreateIndex(0, 0), Count, 1);
    WriteOutput('Fill(1)', '[' + Implode('; ', ', ', IntToStr) + ']');
    Count := CreateIndex(3, 3);
    WriteOutput('Count := CreateIndex(3, 3)', '[' + Implode('; ', ', ', IntToStr) + ']');
    WriteOutput('Count [Y, X]', IntToStr(Count.Y) + ', ' + IntToStr(Count.X));
    Clear;
    WriteOutput('Clear', '[' + Implode('; ', ', ', IntToStr) + ']');
    WriteOutput('Count [Y, X]', IntToStr(Count.Y) + ', ' + IntToStr(Count.X));
  finally
    Free;
  end;
end;

procedure TMainForm.ButtonStreamByteClick(Sender: TObject);
var
  Stream: TMemoryStreamByte;
  I: Integer;
  ByteArray: array of Byte;
  ByteArrayText: string;
begin
  ListViewOutput.Clear;
  LabelTestName.Caption := 'TStreamByte test';
  Stream := TMemoryStreamByte.Create;
  with Stream do try
    WriteOutput('Size := ', IntToStr(Stream.Size));
    Write(1);
    WriteOutput('Write(1)', '');
    WriteOutput('Size, Position', IntToStr(Stream.Size) + ', ' + IntToStr(Stream.Position));
    WriteArray([2, 3, 4]);
    WriteOutput('WriteArray([2, 3, 4])', '');
    WriteOutput('Size, Position', IntToStr(Stream.Size) + ', ' + IntToStr(Stream.Position));
    Position := 1;
    WriteOutput('Position := 1', '');
    WriteOutput('Size, Position', IntToStr(Stream.Size) + ', ' + IntToStr(Stream.Position));
    WriteOutput('Read', IntToStr(Read));
    WriteOutput('Size, Position', IntToStr(Stream.Size) + ', ' + IntToStr(Stream.Position));
    ByteArray := ReadArray(2);
    ByteArrayText := '[';
    for I := 0 to Length(ByteArray) - 1 do begin
      ByteArrayText := ByteArrayText + IntToStr(ByteArray[I]);
      if I < Length(ByteArray) - 1 then ByteArrayText := ByteArrayText + ', ';
    end;
    ByteArrayText := ByteArrayText + ']';
    WriteOutput('ReadArray', ByteArrayText);
    WriteOutput('Size, Position', IntToStr(Stream.Size) + ', ' + IntToStr(Stream.Position));
  finally
    Free;
  end;
end;

function ObjectToStr(Obj: TObject): string;
begin
  Result := Obj.ClassName;
end;

procedure TMainForm.ButtonListObjectClick(Sender: TObject);
var
  List: TListObject;
  I: Integer;
begin
  ListViewOutput.Clear;
  LabelTestName.Caption := 'TListObject test';
  List := TListObject.Create;
  with List do try
    AddArray([TObject.Create, TObject.Create, TObject.Create, TObject.Create]);
    WriteOutput('AddArray([TObject.Create, TObject.Create, TObject.Create, TObject.Create])', Implode(',', ObjectToStr));
    Clear;
    WriteOutput('Clear', Implode(',', ObjectToStr));
    for I := 0 to 10 do Add(TObject.Create);
    WriteOutput('for I := 0 to 10 do Add(TObject.Create)', Implode(',', ObjectToStr));
    WriteOutput('Count', IntToStr(Count));
    Reverse;
    WriteOutput('Reverse', Implode(',', ObjectToStr));
    MoveItems(3, 2, 3);
    WriteOutput('MoveItems(3, 2, 3)', Implode(',', ObjectToStr));
  finally
    Free;
  end;
end;

procedure TMainForm.ButtonQueueIntegerClick(Sender: TObject);
var
  Queue: TQueueInteger;
  I: Integer;
begin
  ListViewOutput.Clear;
  LabelTestName.Caption := 'TQueueInteger test';
  Queue := TQueueInteger.Create;
  with Queue do try
    Enqueue(1);
    Enqueue(2);
    Enqueue(3);
    WriteOutput('Enqueue(1),Enqueue(2),Enqueue(3) ', List.Implode(',', IntToStr));
    Enqueue(4);
    WriteOutput('Enqueue(4)', List.Implode(',', IntToStr));
    WriteOutput('Dequeued item', IntToStr(Dequeue));
    WriteOutput('Dequeue', List.Implode(',', IntToStr));
  finally
    Free;
  end;
end;

function StringPairToStr(Pair: TPairStringString): string;
begin
  Result := Pair.Key + ':' + Pair.Value;
end;

procedure TMainForm.ButtonDictionaryStringClick(Sender: TObject);
var
  Dictionary: TDictionaryStringString;
begin
  ListViewOutput.Clear;
  LabelTestName.Caption := 'TDictionaryString test';
  Dictionary := TDictionaryStringString.Create;
  with Dictionary do try
    Add('Key1', 'Value1');
    Add('Key2', 'Value2');
    Add('Key3', 'Value3');
    WriteOutput('Add(''Key1'', ''Value1''),Add(''Key1'', ''Value1''),Add(''Key1'', ''Value1'')', Implode(',', StringPairToStr));
    WriteOutput('Values[Key2]', Values['Key2']);
    WriteOutput('Values[Key2] = None');
    Values['Key2'] := 'None';
    WriteOutput('Values[Key2]', Values['Key2']);
    WriteOutput('Values[Key0]', Values['Key0']);
    WriteOutput('Keys[2]', Keys[2]);
  finally
    Free;
  end;
end;

function CharToStr(Value: Char): string;
begin
  Result := Value;
end;

procedure TMainForm.ButtonCharListClick(Sender: TObject);
var
  List: TListChar;
  List2: TListChar;
begin
  ListViewOutput.Clear;
  LabelTestName.Caption := 'TListChar test';
  List := TListChar.Create;
  List2 := TListChar.Create;
  with List do try
    AddArray([' ', ' ', 'A', 'b', 'c', 'd', ' ']);
    WriteOutput('AddArray(['' '', '' '', ''A'', ''b'', ''c'', ''d'', '' ''])',
      '''' + Implode('', CharToStr) + '''');
    Reverse;
    WriteOutput('Reverse', '''' + Implode('', CharToStr) + '''');
    TrimLeft;
    WriteOutput('TrimLeft', '''' + Implode('', CharToStr) + '''');
    TrimRight;
    WriteOutput('TrimRight', '''' + Implode('', CharToStr) + '''');
    UpperCase;
    WriteOutput('UpperCase', '''' + Implode('', CharToStr) + '''');
    LowerCase;
    WriteOutput('LowerCase', '''' + Implode('', CharToStr) + '''');
    WriteOutput('IndexOf(''c'')', IntToStr(IndexOf('c')));
    List2.AddArray(['c', 'b']);
    WriteOutput('IndexOfList(''cb'')', IntToStr(IndexOfList(List2)));
  finally
    List2.Free;
    Free;
  end;
end;

procedure TMainForm.ButtonBenchmarkListStringClick(Sender: TObject);
var
  List: TListString;
  List2: TStringList;
  StartTime: TDateTime;
  I: Integer;
const
  SampleText: string = 'text';
  SampleCount: Integer = 100000;
begin
  LabelTestName.Caption := 'Generic specialized TListString vs. classic non-generic TStringList benchmark';
  ListViewOutput.Clear;
  try
    UpdateButtonState(False);
    List := TListString.Create;
    List2 := TStringList.Create;

    StartTime := Now;
    repeat
      List.Add(SampleText);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListString.Add', IntToStr(List.Count) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    StartTime := Now;
    repeat
      List2.Add(SampleText);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TStringList.Add', IntToStr(List2.Count) + ' ops');
    List2.Clear;
    Application.ProcessMessages;

    StartTime := Now;
    repeat
      List.Insert(0, SampleText);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListString.Insert', IntToStr(List.Count) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    StartTime := Now;
    repeat
      List2.Insert(0, SampleText);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TStringList.Insert', IntToStr(List2.Count) + ' ops');
    List2.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List.Add(SampleText);
    StartTime := Now;
    I := 0;
    repeat
      List.Delete(0);
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListString.Delete', IntToStr(I) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List2.Add(SampleText);
    StartTime := Now;
    I := 0;
    repeat
      List2.Delete(0);
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TStringList.Delete', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List.Add(SampleText);
    StartTime := Now;
    I := 0;
    repeat
      List.Move(Round(SampleCount * 0.3), Round(SampleCount * 0.7));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListString.Move', IntToStr(I) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List2.Add(SampleText);
    StartTime := Now;
    I := 0;
    repeat
      List2.Move(Round(SampleCount * 0.3), Round(SampleCount * 0.7));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TStringList.Move', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List.Add(SampleText);
    StartTime := Now;
    I := 0;
    repeat
      List.Exchange(Round(SampleCount * 0.3), Round(SampleCount * 0.7));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListString.Exchange', IntToStr(I) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
    List2.Add(SampleText);
    StartTime := Now;
    I := 0;
    repeat
      List2.Exchange(Round(SampleCount * 0.3), Round(SampleCount * 0.7));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TStringList.Exchange', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List.Add(SampleText + IntToStr(I));
    StartTime := Now;
    I := 0;
    repeat
      List.IndexOf(SampleText + IntToStr(I mod List.Count));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListString.IndexOf', IntToStr(I) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List2.Add(SampleText + IntToStr(I));
    StartTime := Now;
    I := 0;
    repeat
      List2.IndexOf(SampleText + IntToStr(I mod List2.Count));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TStringList.IndexOf', IntToStr(I) + ' ops');
    Application.ProcessMessages;

  finally
    UpdateButtonState(True);
    List.Free;
    List2.Free;
  end;
end;

procedure TMainForm.ButtonBenchmarkDictionaryClick(Sender: TObject);
var
  Dictionary: TDictionaryStringString;
  Dictionary2: TStringList;
  StartTime: TDateTime;
  I: Integer;
  R: string;
begin
  LabelTestName.Caption := 'Generic specialized TDictionaryStringString vs. classic non-generic TStringList benchmark';
  ListViewOutput.Clear;
  try
    UpdateButtonState(False);
    Dictionary := TDictionaryStringString.Create;
    Dictionary2 := TStringList.Create;
    Dictionary2.NameValueSeparator := '|';

    I := 0;
    StartTime := Now;
    repeat
      Dictionary.Add(IntToStr(I), IntToStr(I));
      I := I + 1;
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TDictionaryStringString.Add', IntToStr(Dictionary.Count) + ' ops');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      Dictionary2.Add(IntToStr(I) + Dictionary2.NameValueSeparator + IntToStr(I));
      I := I + 1;
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TStringList.Add', IntToStr(Dictionary2.Count) + ' ops');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary.Values[IntToStr(I mod Dictionary.Count)];
      I := I + 1;
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TDictionaryStringString.Values', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary2.Values[IntToStr(I mod Dictionary2.Count)];
      I := I + 1;
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TStringList.Values', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary.Keys[I mod Dictionary.Count];
      I := I + 1;
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TDictionaryStringString.Keys', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary2.Names[I mod Dictionary2.Count];
      I := I + 1;
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TStringList.Keys(Names)', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary.Items[I mod Dictionary.Count].Value;
      I := I + 1;
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TDictionaryStringString.Items', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary2.ValueFromIndex[I mod Dictionary2.Count];
      I := I + 1;
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TStringList.Items(ValueFromIndex)', IntToStr(I) + ' ops');
    Application.ProcessMessages;

  finally
    UpdateButtonState(True);
    Dictionary.Free;
    Dictionary2.Free;
  end;
end;

procedure TMainForm.ButtonBenchmarkListPointerClick(Sender: TObject);
var
  List: TListPointer;
  List2: TFPList;
  StartTime: TDateTime;
  I: Integer;
const
  SampleCount: Integer = 100000;
begin
  LabelTestName.Caption := 'Generic specialized TListObject vs. classic non-generic TFPList benchmark';
  ListViewOutput.Clear;
  try
    UpdateButtonState(False);
    List := TListPointer.Create;
    List2 := TFPList.Create;

    WriteOutput('TListPointer.InstanceSize', IntToStr(TListPointer.InstanceSize) + ' bytes');
    WriteOutput('TFPList.InstanceSize', IntToStr(TFPList.InstanceSize) + ' bytes');

    StartTime := Now;
    repeat
      List.Add(Pointer(1));
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListPointer.Add', IntToStr(List.Count) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    StartTime := Now;
    repeat
      List2.Add(Pointer(1));
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TFPList.Add', IntToStr(List2.Count) + ' ops');
    List2.Clear;
    Application.ProcessMessages;

    StartTime := Now;
    repeat
      List.Insert(0, Pointer(1));
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListPointer.Insert', IntToStr(List.Count) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    StartTime := Now;
    repeat
      List2.Insert(0, Pointer(1));
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TFPList.Insert', IntToStr(List2.Count) + ' ops');
    List2.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List.Delete(0);
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListPointer.Delete', IntToStr(I) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List2.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List2.Delete(0);
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TFPList.Delete', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List.Move(Round(SampleCount * 0.3), Round(SampleCount * 0.7));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListPointer.Move', IntToStr(I) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
    List2.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List2.Move(Round(SampleCount * 0.3), Round(SampleCount * 0.7));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TFPList.Move', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List.Exchange(Round(SampleCount * 0.3), Round(SampleCount * 0.7));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListPointer.Exchange', IntToStr(I) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
    List2.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List2.Exchange(Round(SampleCount * 0.3), Round(SampleCount * 0.7));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TFPList.Exchange', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List.IndexOf(Pointer(I mod List.Count));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListPointer.IndexOf', IntToStr(I) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
    List2.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List2.IndexOf(Pointer(I mod List2.Count));
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TFPList.IndexOf', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List[I mod List.Count] := Pointer(1);
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListPointer[I] write', IntToStr(I) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
    List2.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List2[I mod List2.Count] := Pointer(1);
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TFPList[I] write', IntToStr(I) + ' ops');
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
      List.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List[I mod List.Count];
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TListPointer[I] read', IntToStr(I) + ' ops');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to SampleCount - 1 do
    List2.Add(Pointer(1));
    StartTime := Now;
    I := 0;
    repeat
      List2[I mod List2.Count];
      Inc(I);
    until (Now - StartTime) > MeasureDuration;
    WriteOutput('TFPList[I] read', IntToStr(I) + ' ops');
    Application.ProcessMessages;
  finally
    UpdateButtonState(True);
    List.Free;
    List2.Free;
  end;
end;

function StrToStr(Value: string): string;
begin
  Result := Value;
end;

procedure TMainForm.ButtonStringListClick(Sender: TObject);
var
  List: TListString;
begin
  ListViewOutput.Clear;
  WriteOutput('TListString test');
  List := TListString.Create;
  with List do try
    AddArray(['One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven']);
    WriteOutput('Count', IntToStr(Count));
    WriteOutput('Implode', Implode(',', StrToStr));
    WriteOutput('Reverse');
    Reverse;
    WriteOutput('Implode', Implode(',', StrToStr));
    WriteOutput('First', First);
    WriteOutput('Last', Last);
    MoveItems(2, 3, 3);
    WriteOutput('Implode', Implode(',', StrToStr));
    InsertCount(0, 3);
    WriteOutput('InsertCount(0, 3)', Implode(',', StrToStr));
    Fill(0, 3, 'Zero');
    WriteOutput('Fill(0, 3, ''Zero'')', Implode(',', StrToStr));
  finally
    Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TMainForm.UpdateButtonState(Enabled: Boolean);
begin
  ButtonBenchmarkDictionary.Enabled := Enabled;
  ButtonBenchmarkListString.Enabled := Enabled;
  ButtonCharList.Enabled := Enabled;
  ButtonDictionaryString.Enabled := Enabled;
  ButtonIntegerList.Enabled := Enabled;
  ButtonListObject.Enabled := Enabled;
  ButtonMatrixInteger.Enabled := Enabled;
  ButtonQueueInteger.Enabled := Enabled;
  ButtonStringList.Enabled := Enabled;
end;

procedure TMainForm.WriteOutput(Text1: string = ''; Text2: string = '');
var
  NewItem: TListItem;
begin
  NewItem := ListViewOutput.Items.Add;
  NewItem.Caption := Text1;
  NewItem.SubItems.Add(Text2);
end;

end.

