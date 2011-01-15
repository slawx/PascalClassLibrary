unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, UDynNumber, Math;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    ListView1: TListView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
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
  Line: string;
  NewItem: TListItem;
begin
  try
    N := TDynamicNumber.Create;
    ListView1.BeginUpdate;
    ListView1.Clear;
    for I := 0 to 16 do begin
      N.Stream.Size := 0;
      N.Write(I);
      Line := '';
      N.Stream.Position := 0;
      for J := 0 to N.Stream.Size - 1 do
        Line := Line + IntToStr(Integer(N.Stream.ReadBit));
      NewItem := ListView1.Items.Add;
      NewItem.Caption := IntToStr(I);
      NewItem.SubItems.Add('');
      NewItem.SubItems.Add(Line);
    end;
  finally
    ListView1.EndUpdate;
    N.Free;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  I: Integer;
  Count: Integer;
  Parts: array of Integer;
  PartIndex: Integer;
  MaxValue: Integer;
  J: Integer;
  Line: string;
  NewItem: TListItem;
  N: TDynamicNumber;
begin
  Count := 1;
  SetLength(Parts, Count);
  try
    N := TDynamicNumber.Create;
    ListView1.BeginUpdate;
    ListView1.Clear;
    for I := 0 to 1000 do begin

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
    NewItem.SubItems.Add(FloatToStr(1 - (J / N.Stream.Size)));
    Line := '';
    N.Stream.Position := 0;
    for J := 0 to N.Stream.Size - 1 do
      Line := Line + IntToStr(Integer(N.Stream.ReadBit));
    NewItem.SubItems.Add(Line);

    // Increment value
    PartIndex := Count - 1;
    repeat
      Parts[PartIndex] := Parts[PartIndex] + 1;
      if PartIndex > 0 then MaxValue := 1 shl (Parts[PartIndex - 1] + 1)
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
  finally
    ListView1.EndUpdate;
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
