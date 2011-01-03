unit UMainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, SpecializedList, SpecializedDictionary, SpecializedQueue,
  DateUtils, SpecializedMatrix;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonBenchmarkDictionary: TButton;
    ButtonListObject: TButton;
    ButtonBenchmarkList: TButton;
    ButtonCharList: TButton;
    ButtonMatrixInteger: TButton;
    ButtonQueueInteger: TButton;
    ButtonDictionaryString: TButton;
    ButtonIntegerList: TButton;
    ButtonStringList: TButton;
    LabelTestName: TLabel;
    ListViewOutput: TListView;
    procedure ButtonBenchmarkDictionaryClick(Sender: TObject);
    procedure ButtonBenchmarkListClick(Sender: TObject);
    procedure ButtonCharListClick(Sender: TObject);
    procedure ButtonDictionaryStringClick(Sender: TObject);
    procedure ButtonIntegerListClick(Sender: TObject);
    procedure ButtonMatrixIntegerClick(Sender: TObject);
    procedure ButtonListObjectClick(Sender: TObject);
    procedure ButtonQueueIntegerClick(Sender: TObject);
    procedure ButtonStringListClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    Bitmap: TBitmap;
    procedure WriteOutput(Text1: string = ''; Text2: string = '');
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
end;

procedure TMainForm.ButtonIntegerListClick(Sender: TObject);
var
  List: TListInteger;
  I: Integer;
begin
  ListViewOutput.Clear;
  LabelTestName.Caption := 'TListInteger test';
  List := TListInteger.Create;
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
  finally
    Free;
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

procedure TMainForm.ButtonBenchmarkListClick(Sender: TObject);
var
  List: TListPointer;
  List2: TList;
  StartTime: TDateTime;
  I: Integer;
begin
  LabelTestName.Caption := 'Generic specialized TListObject vs. classic non-generic TList benchmark';
  ListViewOutput.Clear;
  try
    List := TListPointer.Create;
    List2 := TList.Create;

    StartTime := Now;
    repeat
      List.Add(1);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TListPointer.Add', IntToStr(List.Count) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;

    StartTime := Now;
    repeat
      List2.Add(1);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TList.Add', IntToStr(List2.Count) + ' ops/sec');
    List2.Clear;
    Application.ProcessMessages;

    StartTime := Now;
    repeat
      List.Insert(0, 1);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TListPointer.Insert', IntToStr(List.Count) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;

    StartTime := Now;
    repeat
      List2.Insert(0, 1);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TList.Insert', IntToStr(List2.Count) + ' ops/sec');
    List2.Clear;
    Application.ProcessMessages;

    for I := 0 to 1000000 do
      List.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List.Delete(0);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TListPointer.Delete', IntToStr(I) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to 1000000 do
      List2.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List2.Delete(0);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TList.Delete', IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    for I := 0 to 1000000 do
      List.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List.Move(300000, 700000);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TListPointer.Move', IntToStr(I) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to 1000000 do
    List2.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List2.Move(300000, 700000);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TList.Move', IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    for I := 0 to 1000000 do
      List.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List.Exchange(300000, 700000);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TListPointer.Exchange', IntToStr(I) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to 1000000 do
    List2.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List2.Exchange(300000, 700000);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TList.Exchange', IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    for I := 0 to 1000000 do
      List.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List.IndexOf(Pointer(I mod List.Count));
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TListPointer.IndexOf', IntToStr(I) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to 1000000 do
    List2.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List2.IndexOf(Pointer(I mod List2.Count));
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteOutput('TList.IndexOf', IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

  finally
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
    Dictionary := TDictionaryStringString.Create;
    Dictionary2 := TStringList.Create;
    Dictionary2.NameValueSeparator := '|';

    I := 0;
    StartTime := Now;
    repeat
      Dictionary.Add(IntToStr(I), IntToStr(I));
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteOutput('TDictionaryStringString.Add', IntToStr(Dictionary.Count) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      Dictionary2.Add(IntToStr(I) + Dictionary2.NameValueSeparator + IntToStr(I));
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteOutput('TStringList.Add', IntToStr(Dictionary2.Count) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary.Values[IntToStr(I mod Dictionary.Count)];
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteOutput('TDictionaryStringString.Values', IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary2.Values[IntToStr(I mod Dictionary2.Count)];
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteOutput('TStringList.Values', IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary.Keys[I mod Dictionary.Count];
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteOutput('TDictionaryStringString.Keys', IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary2.Names[I mod Dictionary2.Count];
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteOutput('TStringList.Keys(Names)', IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary.Items[I mod Dictionary.Count].Value;
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteOutput('TDictionaryStringString.Items', IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary2.ValueFromIndex[I mod Dictionary2.Count];
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteOutput('TStringList.Items(ValueFromIndex)', IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

  finally
    Dictionary.Free;
    Dictionary2.Free;
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
  finally
    Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
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

