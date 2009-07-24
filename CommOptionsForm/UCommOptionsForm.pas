unit UCommOptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CommPort, ExtCtrls;

type
  TCommOptionsForm = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Label3: TLabel;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    Label6: TLabel;
    Button1: TButton;
    Button2: TButton;
    Bevel1: TBevel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    CommPort: TCommPort;
  public
    procedure ShowForm(CommPort: TCommPort);
  end;

var
  CommOptionsForm: TCommOptionsForm;

implementation

uses UMainForm;

{$R *.dfm}

procedure TCommOptionsForm.Button1Click(Sender: TObject);
begin
  with CommPort do begin
    Open := False;
    if (ComboBox1.Items.Count > 0) and (ComboBox1.ItemIndex <> -1) then
      ComNumber := StrToInt(Copy(ComboBox1.Items[ComboBox1.ItemIndex], 4, 3))
      else ComNumber := 1;
    BaudRate := TBaudRate(ComboBox2.ItemIndex);
    Parity := TParity(ComboBox3.ItemIndex);
    DataBits := TDataBits(ComboBox4.ItemIndex + 4);
    StopBits := TStopBits(ComboBox5.ItemIndex);
    FlowControl := TFlowControl(ComboBox6.ItemIndex);
    Open := True;
  end;
  Close;
end;

procedure TCommOptionsForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TCommOptionsForm.ShowForm(CommPort: TCommPort);
var
  I: Integer;
  TestPort: TCommPort;
//  Index: Integer;
begin
  Self.CommPort := CommPort;
  TestPort := TCommPort.Create(nil);
  ComboBox1.Clear;
  for I := 1 to 255 do with TestPort do begin
    ComNumber := I;
    Open := True;
    if Open or ((I = CommPort.ComNumber) and (CommPort.Open)) then begin
      ComboBox1.AddItem('COM' + IntToStr(I), Pointer(I));
      if I = CommPort.ComNumber then ComboBox1.ItemIndex := ComboBox1.Items.Count - 1;
    end;
    Open := False;
  end;
  TestPort.Free;

  with CommPort do begin
    ComboBox2.ItemIndex := Integer(BaudRate);
    ComboBox3.ItemIndex := Integer(Parity);
    ComboBox4.ItemIndex := Integer(DataBits) - 4;
    ComboBox5.ItemIndex := Integer(StopBits);
    ComboBox6.ItemIndex := Integer(FlowControl);
  end;
  ShowModal;
end;

end.
