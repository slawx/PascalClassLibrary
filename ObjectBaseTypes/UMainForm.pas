unit UMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UObjectString, UObjectInteger, UObjectBoolean, StdCtrls;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
var
  Operand1: TBoolean;
  Operand2: TBoolean;
  Result: TBoolean;
  Int: TInteger;
begin
  with Memo1, Lines do begin
    Clear;
    Operand1 := TBoolean.Create;
    Operand2 := TBoolean.Create;
    Result := TBoolean.Create;

    // Assign
    Operand1.Value := True;
    Result.Assign(Operand1);
    Add(BoolToStr(Result.Value) + ' := ' + BoolToStr(Operand1.Value));

    Operand1.Value := False;
    Result.Assign(Operand1);
    Add(BoolToStr(Result.Value) + ' := ' + BoolToStr(Operand1.Value));

    // EqualTo
    Operand1.Value := True;
    Result.Value := True;
    Result.EqualTo(Operand1);
    Add(BoolToStr(Result.Value) + ' := ' + BoolToStr(Operand1.Value));

    // OrTo
    Result.Value := True;
    Operand1.Value := True;
    Result.OrTo(Operand1);
    Add(BoolToStr(Result.Value) + ' := ' + BoolToStr(Operand1.Value));
  end;
end;

procedure TMainForm.Button4Click(Sender: TObject);
var
  Operand1: TString;
  Operand2: TString;
  Result: TString;
  Int: TInteger;
  Bool: TBoolean;
begin
  with Memo1, Lines do begin
    Clear;
    Operand1 := TString.Create;
    Operand2 := TString.Create;
    Result := TString.Create;

    // EqualTo
    Result.Value := 'Ab12=';
    Operand1.Value := 'Ab12=';
    Bool := Result.EqualTo(Operand1);
    Add(BoolToStr(Bool.Value) + ' := ' + Result.Value + '.EqualTo(' + Operand1.Value + ')');

    Result.Value := 'Ab12=';
    Operand1.Value := 'Ab12d=';
    Bool := Result.EqualTo(Operand1);
    Add(BoolToStr(Bool.Value) + ' := ' + Result.Value + '.EqualTo(' + Operand1.Value + ')');

    // Length
    Result.Value := 'ABCD';
    Int := Result.Length;
    Add(IntToStr(Int.Value) + ' := ' + Result.Value + '.Length');

    // Reverse
    Operand1.Value := 'ABCD';
    Result.Assign(Operand1);
    Result.Reverse;
    Add(Result.Value + ' := ' + Operand1.Value + '.Reverse');

    // Pos
    Operand1.Value := 'ABCD';
    Operand2.Value := 'C';
    Int := Operand1.Pos(Operand2);
    Add(IntToStr(Int.Value) + ' := ' + Operand1.Value + '.Pos(' + Operand2.Value + ')');
  end;
end;

end.
