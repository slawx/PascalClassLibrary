unit UMainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, SpecializedList, SpecializedDictionary, SpecializedQueue,
  DateUtils, ListObject;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonBenchmarkDictionary: TButton;
    ButtonListObject: TButton;
    ButtonBenchmarkList: TButton;
    ButtonCharList: TButton;
    ButtonQueueInteger: TButton;
    ButtonDictionaryString: TButton;
    ButtonIntegerList: TButton;
    ButtonStringList: TButton;
    MemoOutput: TMemo;
    procedure ButtonBenchmarkDictionaryClick(Sender: TObject);
    procedure ButtonBenchmarkListClick(Sender: TObject);
    procedure ButtonCharListClick(Sender: TObject);
    procedure ButtonDictionaryStringClick(Sender: TObject);
    procedure ButtonIntegerListClick(Sender: TObject);
    procedure ButtonListObjectClick(Sender: TObject);
    procedure ButtonQueueIntegerClick(Sender: TObject);
    procedure ButtonStringListClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    procedure WriteLn(Text: string);
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
  MemoOutput.Clear;
  WriteLn('TListInteger test');
  List := TListInteger.Create;
  with List do try
    AddArray([10, 20, 30, 40]);
    WriteLn('Implode: ' + Implode(',', IntToStr));
    Clear;
    for I := 0 to 10 do Add(I);
    WriteLn('Implode: ' + Implode(',', IntToStr));
    WriteLn('Count: ' + IntToStr(Count));
    WriteLn('Implode: ' + Implode(',', IntToStr));
    WriteLn('Reverse');
    Reverse;
    WriteLn('Implode: ' + Implode(',', IntToStr));
    WriteLn('First: ' + IntToStr(First));
    WriteLn('Last: ' + IntToStr(Last));
    MoveItems(3, 2, 3);
    WriteLn('Implode: ' + Implode(',', IntToStr));
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
  MemoOutput.Clear;
  WriteLn('TListObject test');
  List := TListObject.Create;
  with List do try
    AddArray([TObject.Create, TObject.Create, TObject.Create, TObject.Create]);
    WriteLn('Implode: ' + Implode(',', ObjectToStr));
    Clear;
    for I := 0 to 10 do Add(TObject.Create);
    WriteLn('Implode: ' + Implode(',', ObjectToStr));
    WriteLn('Count: ' + IntToStr(Count));
    WriteLn('Implode: ' + Implode(',', ObjectToStr));
    WriteLn('Reverse');
    Reverse;
    WriteLn('Implode: ' + Implode(',', ObjectToStr));
    MoveItems(3, 2, 3);
    WriteLn('Implode: ' + Implode(',', ObjectToStr));
  finally
    Free;
  end;
end;

procedure TMainForm.ButtonQueueIntegerClick(Sender: TObject);
var
  Queue: TQueueInteger;
  I: Integer;
begin
  MemoOutput.Clear;
  WriteLn('TQueueInteger test');
  Queue := TQueueInteger.Create;
  with Queue do try
    Enqueue(1);
    Enqueue(2);
    Enqueue(3);
    WriteLn('Implode: ' + List.Implode(',', IntToStr));
    WriteLn('Enqueue: 4');
    Enqueue(4);
    WriteLn('Implode: ' + List.Implode(',', IntToStr));
    WriteLn('Dequeue: ' + IntToStr(Dequeue));
    WriteLn('Implode: ' + List.Implode(',', IntToStr));
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
  MemoOutput.Clear;
  WriteLn('TDictionaryString test');
  Dictionary := TDictionaryStringString.Create;
  with Dictionary do try
    Add('Key1', 'Value1');
    Add('Key2', 'Value2');
    Add('Key3', 'Value3');
    WriteLn('Implode: ' + Implode(',', StringPairToStr));
    WriteLn('Values[Key2]: ' + Values['Key2']);
    WriteLn('Values[Key2] = None');
    Values['Key2'] := 'None';
    WriteLn('Values[Key2]: ' + Values['Key2']);
    WriteLn('Values[Key0]: ' + Values['Key0']);
    WriteLn('Keys[2]: ' + Keys[2]);
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
begin
  MemoOutput.Clear;
  WriteLn('TListChar test');
  List := TListChar.Create;
  with List do try
    AddArray([' ', ' ', 'A', 'b', 'c', 'd', ' ']);
    WriteLn('Implode: ''' + Implode('', CharToStr) + '''');
    WriteLn('Implode: ' + Implode('', CharToStr));
    WriteLn('Reverse');
    Reverse;
    WriteLn('Implode: ''' + Implode('', CharToStr) + '''');
    WriteLn('TrimLeft');
    TrimLeft;
    WriteLn('Implode: ''' + Implode('', CharToStr) + '''');
    WriteLn('TrimRight');
    TrimRight;
    WriteLn('Implode: ''' + Implode('', CharToStr) + '''');
    WriteLn('UpperCase');
    UpperCase;
    WriteLn('Implode: ''' + Implode('', CharToStr) + '''');
  finally
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
  MemoOutput.Clear;
  try
    List := TListPointer.Create;
    WriteLn('TListPointer...');
    StartTime := Now;
    repeat
      List.Add(1);
    until (Now - StartTime) > OneSecond;
    WriteLn('Add: ' + IntToStr(List.Count) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;

    StartTime := Now;
    repeat
      List.Insert(0, 1);
    until (Now - StartTime) > OneSecond;
    WriteLn('Insert: ' + IntToStr(List.Count) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to 1000000 do
      List.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List.Delete(0);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteLn('Delete: ' + IntToStr(I) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to 1000000 do
      List.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List.Move(300000, 700000);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteLn('Move: ' + IntToStr(I) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to 1000000 do
      List.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List.Exchange(300000, 700000);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteLn('Exchange: ' + IntToStr(I) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;

    for I := 0 to 1000000 do
      List.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List.IndexOf(Pointer(I mod List.Count));
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteLn('IndexOf: ' + IntToStr(I) + ' ops/sec');
    List.Clear;
    Application.ProcessMessages;
  finally
    List.Free;
  end;

  try
    List2 := TList.Create;
    WriteLn('Test TList...');
    StartTime := Now;
    repeat
      List2.Add(1);
    until (Now - StartTime) > OneSecond;
    WriteLn('Add: ' + IntToStr(List2.Count) + ' ops/sec');
    List2.Clear;
    Application.ProcessMessages;

    StartTime := Now;
    repeat
      List2.Insert(0, 1);
    until (Now - StartTime) > OneSecond;
    WriteLn('Insert: ' + IntToStr(List2.Count) + ' ops/sec');
    List2.Clear;
    Application.ProcessMessages;

    for I := 0 to 1000000 do
      List2.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List2.Delete(0);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteLn('Delete: ' + IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    for I := 0 to 1000000 do
    List2.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List2.Move(300000, 700000);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteLn('Move: ' + IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    for I := 0 to 1000000 do
    List2.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List2.Exchange(300000, 700000);
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteLn('Exchange: ' + IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    for I := 0 to 1000000 do
    List2.Add(1);
    StartTime := Now;
    I := 0;
    repeat
      List2.IndexOf(Pointer(I mod List.Count));
      Inc(I);
    until (Now - StartTime) > OneSecond;
    WriteLn('IndexOf: ' + IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

  finally
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
  MemoOutput.Clear;
  try
    Dictionary := TDictionaryStringString.Create;
    WriteLn('TDictionaryStringString...');
    I := 0;
    StartTime := Now;
    repeat
      Dictionary.Add(IntToStr(I), IntToStr(I));
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteLn('Add pair: ' + IntToStr(Dictionary.Count) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary.Values[IntToStr(I mod Dictionary.Count)];
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteLn('Values: ' + IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary.Keys[I mod Dictionary.Count];
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteLn('Keys: ' + IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary.Items[I mod Dictionary.Count].Value;
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteLn('Values by index: ' + IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;
  finally
    Dictionary.Free;
  end;

  try
    Dictionary2 := TStringList.Create;
    Dictionary2.NameValueSeparator := '|';
    WriteLn('TStringList...');
    I := 0;
    StartTime := Now;
    repeat
      Dictionary2.Add(IntToStr(I) + Dictionary2.NameValueSeparator + IntToStr(I));
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteLn('Add pair: ' + IntToStr(Dictionary2.Count) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary2.Values[IntToStr(I mod Dictionary2.Count)];
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteLn('Values: ' + IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary2.Names[I mod Dictionary2.Count];
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteLn('Keys: ' + IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;

    I := 0;
    StartTime := Now;
    repeat
      R := Dictionary2.ValueFromIndex[I mod Dictionary2.Count];
      I := I + 1;
    until (Now - StartTime) > OneSecond;
    WriteLn('Values by index: ' + IntToStr(I) + ' ops/sec');
    Application.ProcessMessages;
  finally
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
  MemoOutput.Clear;
  WriteLn('TListString test');
  List := TListString.Create;
  with List do try
    AddArray(['One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven']);
    WriteLn('Count: ' + IntToStr(Count));
    WriteLn('Implode: ' + Implode(',', StrToStr));
    WriteLn('Reverse');
    Reverse;
    WriteLn('Implode: ' + Implode(',', StrToStr));
    WriteLn('First: ' + First);
    WriteLn('Last: ' + Last);
    MoveItems(2, 3, 3);
    WriteLn('Implode: ' + Implode(',', StrToStr));
  finally
    Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TMainForm.WriteLn(Text: string);
begin
  MemoOutput.Lines.Add(Text);
end;

end.

