unit UMainForm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ListInteger, ListString;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonIntegerList: TButton;
    ButtonStringList: TButton;
    MemoOutput: TMemo;
    procedure ButtonIntegerListClick(Sender: TObject);
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
  WriteLn('TIntegerList test');
  List := TListInteger.Create;
  with List do try
    SetArray([10, 20, 30, 40]);
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

function StrToStr(Value: string): string;
begin
  Result := Value;
end;

procedure TMainForm.ButtonStringListClick(Sender: TObject);
var
  List: TListString;
begin
  MemoOutput.Clear;
  WriteLn('TStringList test');
  List := TListString.Create;
  with List do try
    SetArray(['One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven']);
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

