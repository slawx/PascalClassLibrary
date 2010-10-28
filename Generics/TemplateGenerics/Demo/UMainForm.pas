unit UMainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ListInteger, ListString, DictionaryString, QueueInteger;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonQueueInteger: TButton;
    ButtonDictionaryString: TButton;
    ButtonIntegerList: TButton;
    ButtonStringList: TButton;
    MemoOutput: TMemo;
    procedure ButtonDictionaryStringClick(Sender: TObject);
    procedure ButtonIntegerListClick(Sender: TObject);
    procedure ButtonQueueIntegerClick(Sender: TObject);
    procedure ButtonStringListClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
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
    WriteLn('Implode: ' + Implode(',', IntToStr));
    WriteLn('Enqueue: 4');
    Enqueue(4);
    WriteLn('Implode: ' + Implode(',', IntToStr));
    WriteLn('Dequeue: ' + IntToStr(Dequeue));
    WriteLn('Implode: ' + Implode(',', IntToStr));
  finally
    Free;
  end;
end;

function StringPairToStr(Pair: TPairString): string;
begin
  Result := Pair.Key + ':' + Pair.Value;
end;

procedure TMainForm.ButtonDictionaryStringClick(Sender: TObject);
var
  Dictionary: TDictionaryString;
begin
  MemoOutput.Clear;
  WriteLn('TDictionaryString test');
  Dictionary := TDictionaryString.Create;
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

