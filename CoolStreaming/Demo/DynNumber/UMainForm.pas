unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Spin, UDynNumber, Math;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    ListView1: TListView;
    PageControl1: TPageControl;
    SpinEdit1: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    DynamicNumber: TDynamicNumber;
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
var
  I: Integer;
  J: Integer;
  C: Integer;
  Parts: array of Integer;
  N: TDynamicNumber;
  NewItem: TListItem;
begin
  try
    N := TDynamicNumber.Create;
    ListView1.BeginUpdate;
    ListView1.Clear;
    for I := 0 to SpinEdit1.Value do begin
      N.Stream.Size := 0;
      N.WriteNumber(I);
      NewItem := ListView1.Items.Add;
      NewItem.Caption := IntToStr(I);
      J := Floor(Log2(I)) + 1;
      NewItem.SubItems.Add(FloatToStr(Round((1 - (J / N.Stream.Size)) * 100) / 100));
      NewItem.SubItems.Add(N.Stream.AsString);
    end;
  finally
    ListView1.EndUpdate;
    N.Free;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  I: Integer;
  II: Integer;
  Count: Integer;
  Parts: array of Integer;
  PartIndex: Integer;
  MaxValue: Integer;
  J: Integer;
  NewItem: TListItem;
  N: TDynamicNumber;
const
  Step = 1;
begin
  Count := 1;
  SetLength(Parts, Count);
  try
    N := TDynamicNumber.Create;
    ListView1.BeginUpdate;
    ListView1.Clear;
    for II := 0 to SpinEdit1.Value do begin
      I := II * Step;
    // Write
    N.Stream.Size := 0;
    for J := 0 to Count - 2 do
      N.Stream.WriteNumber(1, 1);
    N.Stream.WriteNumber(0, 1);
    for PartIndex := 0 to Count - 1 do begin
      if PartIndex > 0 then MaxValue := Parts[PartIndex - 1] + 1
        else MaxValue := 1;
      for J := MaxValue - 1 downto 0 do
        if ((Parts[PartIndex] shr J) and 1) = 1 then N.Stream.WriteNumber(1, 1)
          else N.Stream.WriteNumber(0, 1);
    end;
    NewItem := ListView1.Items.Add;
    NewItem.Caption := IntToStr(I);
    J := Floor(Log2(I)) + 1;
    NewItem.SubItems.Add(FloatToStr(Round((1 - (J / N.Stream.Size)) * 100) / 100));
    NewItem.SubItems.Add(N.Stream.AsString);

    // Increment value
    for J := 0 to Step - 1 do begin
    PartIndex := Count - 1;
    repeat
      Parts[PartIndex] := Parts[PartIndex] + 1;
      if PartIndex > 0 then MaxValue := 1 shl (Parts[PartIndex - 1] + 1 + PartIndex)
        else MaxValue := 2;
      if Parts[PartIndex] >= MaxValue then begin
        Parts[PartIndex] := 0;
        PartIndex := PartIndex - 1;
        if PartIndex < 0 then begin
          Count := Count + 1;
          SetLength(Parts, Count);
          for PartIndex := 0 to Count - 1 do
            Parts[PartIndex] := 0;
          Break;
        end;
      end else Break;
    until False;
    end;
    end;
  finally
    ListView1.EndUpdate;
    N.Free;
  end;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  N: TDynamicNumber;
begin
  try
    N := TDynamicNumber.Create;
    N.Stream.Size := 0;
    N.WriteNumber(StrToInt64(Edit1.Text));
    N.Stream.Position := 0;
    Edit2.Text := N.Stream.AsString;
  finally
    N.Free;
  end;
end;

procedure TMainForm.Button4Click(Sender: TObject);
var
  N: TDynamicNumber;
begin
  try
    N := TDynamicNumber.Create;
    N.Stream.AsString := Edit2.Text;
    Edit3.Text := IntToStr(N.ReadNumber);
  finally
    N.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DynamicNumber := TDynamicNumber.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DynamicNumber.Free;
end;

end.
