unit USerialPortOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, USerialPort;

type

  { TSerialPortOptions }

  TSerialPortOptions = class(TForm)
    Bevel1: TBevel;
    ButtonSet: TButton;
    ButtonCancel: TButton;
    CheckBoxDTR: TCheckBox;
    Label7: TLabel;
    CheckBoxRTS: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure ButtonSetClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    SerialPort: TSerialPort;
  end; 

var
  SerialPortOptions: TSerialPortOptions;

implementation

{ TSerialPortOptions }

procedure TSerialPortOptions.ButtonSetClick(Sender: TObject);
begin
  with SerialPort do begin
    Active := False;
    if ComboBox1.Items.Count = 0 then Name := ''
      else Name := ComboBox1.Items[ComboBox1.ItemIndex];
    BaudRate := TBaudRate(ComboBox2.ItemIndex);
    Parity := TParity(ComboBox3.ItemIndex);
    DataBits := ComboBox4.ItemIndex + 4;
    StopBits := TStopBits(ComboBox5.ItemIndex);
    FlowControl := TFlowControl(ComboBox6.ItemIndex);
    RTS := CheckBoxRTS.Checked;
    DTR := CheckBoxDTR.Checked;
    Active := True;
  end;
  Close;
end;

procedure TSerialPortOptions.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TSerialPortOptions.ComboBox3Change(Sender: TObject);
begin

end;

procedure TSerialPortOptions.FormShow(Sender: TObject);
var
  I: Integer;
  TestPort: TSerialPort;
//  Index: Integer;
begin
  SerialPort.Active := False;
  TestPort := TSerialPort.Create;
  ComboBox1.Clear;
  for I := 1 to 255 do with TestPort do begin
    Name := 'COM' + IntToStr(I);
    Active := True;
    if Active or ((Name = SerialPort.Name) and (SerialPort.Active)) then begin
      ComboBox1.AddItem('COM' + IntToStr(I), TObject(I));
      if Name = SerialPort.Name then
        ComboBox1.ItemIndex := ComboBox1.Items.Count - 1;
    end;
    Active := False;
  end;
  TestPort.Free;
  SerialPort.Active := True;

  if (ComboBox1.Items.Count > 0) and (ComboBox1.ItemIndex = -1) then
    ComboBox1.ItemIndex := 0;

  with SerialPort do begin
    ComboBox2.ItemIndex := Integer(BaudRate);
    ComboBox3.ItemIndex := Integer(Parity);
    ComboBox4.ItemIndex := Integer(DataBits) - 4;
    ComboBox5.ItemIndex := Integer(StopBits);
    ComboBox6.ItemIndex := Integer(FlowControl);
    CheckBoxRTS.Checked := RTS;
    CheckBoxDTR.Checked := DTR;
  end;
end;

initialization
  {$I USerialPortOptions.lrs}

end.

