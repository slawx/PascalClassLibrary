unit UConfirmationForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, URegistry, SpecializedList;

type
  { TConfirmMessage }

  TConfirmMessage = class
    SysName: string;
    Enabled: Boolean;
    Title: string;
    Text: string;
    Buttons: TMsgDlgButtons;
    DefaultAction: TModalResult;
    function DefaultActionText: string;
  end;

  { TConfirmMessageList }

  TConfirmMessageList = class(TListObject)
  public
    constructor Create; override;
    function AddItem(SysName: string; Title, Text: string; Buttons: TMsgDlgButtons;
      DefaultAction: TModalResult = mrNone): TConfirmMessage;
    procedure Register(Message: TConfirmMessage);
    procedure Unregister(Message: TConfirmMessage);
    procedure LoadFromRegistry(Context: TRegistryContext);
    procedure SaveToRegistry(Context: TRegistryContext);
    function SearchByName(Name: string): TConfirmMessage;
  end;

  { TConfirmationForm }

  TConfirmationForm = class(TForm)
    BitBtnOk: TBitBtn;
    BitBtnYes: TBitBtn;
    BitBtnNo: TBitBtn;
    BitBtnCancel: TBitBtn;
    CheckBoxAutoEnabled: TCheckBox;
    Image1: TImage;
    LabelDescription: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ModalResult: Integer;
  public
    ConfirmMessages: TConfirmMessageList;
    function ShowDialog(Message: TConfirmMessage; Parameters: array of const): Integer;
  end;

var
  ConfirmationForm: TConfirmationForm;


implementation

{$R *.lfm}

resourcestring
  SYes = 'Yes';
  SNo = 'No';
  SCancel = 'Cancel';
  SOk = 'Ok';
  SAbort = 'Abort';
  SRetry = 'Retry';
  SIgnore = 'Ignore';
  SClose = 'Close';
  SDoSameActionNextTime = 'Do same action next time';
//  SConfirmMessageNotFound = 'Confirm message id %s not found';


procedure TConfirmationForm.FormCreate(Sender: TObject);
begin
  ConfirmMessages := TConfirmMessageList.Create;
  ConfirmMessages.OwnsObjects := False;
end;

procedure TConfirmationForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ConfirmMessages);
end;

procedure TConfirmationForm.FormShow(Sender: TObject);
begin
  BitBtnOk.Caption := SOk;
  BitBtnCancel.Caption := SCancel;
  BitBtnNo.Caption := SNo;
  BitBtnYes.Caption := SYes;
  CheckBoxAutoEnabled.Caption := SDoSameActionNextTime;
end;

function TConfirmationForm.ShowDialog(Message: TConfirmMessage; Parameters: array of const): Integer;
var
  RightPos: Integer;
const
  Space = 10;
begin
  with Message do begin
    RightPos := Width;
    if mbCancel in Buttons then begin
      Dec(RightPos, BitBtnCancel.Width + Space);
      BitBtnCancel.Left := RightPos;
      BitBtnCancel.Visible := True;
    end else BitBtnCancel.Visible := False;
    if mbNo in Buttons then begin
      Dec(RightPos, BitBtnNo.Width + Space);
      BitBtnNo.Left := RightPos;
      BitBtnNo.Visible := True;
    end else BitBtnNo.Visible := False;
    if mbYes in Buttons then begin
      Dec(RightPos, BitBtnYes.Width + Space);
      BitBtnYes.Left := RightPos;
      BitBtnYes.Visible := True;
    end else BitBtnYes.Visible := False;
    if mbOk in Buttons then begin
      Dec(RightPos, BitBtnOk.Width + Space);
      BitBtnOk.Left := RightPos;
      BitBtnOk.Visible := True;
    end else BitBtnOk.Visible := False;

    Caption := Title;
    LabelDescription.Caption := Format(Text, Parameters);

    CheckBoxAutoEnabled.Checked := False;

    if Enabled or (DefaultAction = mrNone) then ModalResult := ShowModal
      else ModalResult := DefaultAction;

    if CheckBoxAutoEnabled.Checked then begin
      Enabled := False;
      DefaultAction := ModalResult;
    end;
    Result := ModalResult;
  end;
end;

{ TConfirmMessageList }

constructor TConfirmMessageList.Create;
begin
  inherited;
end;

function TConfirmMessageList.AddItem(SysName: string; Title, Text: string;
  Buttons: TMsgDlgButtons; DefaultAction: TModalResult = mrNone): TConfirmMessage;
var
  NewMessage: TConfirmMessage;
begin
  NewMessage := TConfirmMessage.Create;
  NewMessage.SysName := SysName;
  NewMessage.Title := Title;
  NewMessage.Text := Text;
  NewMessage.Enabled := DefaultAction = mrNone;
  NewMessage.DefaultAction := DefaultAction;
  NewMessage.Buttons := Buttons;
  Add(TObject(NewMessage));
  Result := NewMessage;
end;

procedure TConfirmMessageList.Register(Message: TConfirmMessage);
begin
  Add(Message);
end;

procedure TConfirmMessageList.Unregister(Message: TConfirmMessage);
begin
  Remove(Message);
end;

procedure TConfirmMessageList.LoadFromRegistry(Context: TRegistryContext);
var
  I: Integer;
begin
  with TRegistryEx.Create do
    try
      RootKey := Context.RootKey;
      for I := 0 to Count - 1 do
      with TConfirmMessage(Items[I]) do
      begin
        OpenKey(Context.Key + '\' + SysName, True);
        Enabled := ReadBoolWithDefault('Enabled', Enabled);
        DefaultAction := ReadIntegerWithDefault('DefaultAction', DefaultAction);
      end;
  finally
    Free;
  end;
end;

procedure TConfirmMessageList.SaveToRegistry(Context: TRegistryContext);
var
  I: Integer;
begin
  with TRegistryEx.Create do
    try
      RootKey := Context.RootKey;
      for I := 0 to Count - 1 do
      with TConfirmMessage(Items[I]) do
      begin
        OpenKey(Context.Key + '\' + SysName, True);
        WriteBool('Enabled', Enabled);
        WriteInteger('DefaultAction', DefaultAction);
      end;
  finally
    Free;
  end;
end;

function TConfirmMessageList.SearchByName(Name: string): TConfirmMessage;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (TConfirmMessage(Items[I]).SysName <> Name) do Inc(I);
  if I < Count then Result := TConfirmMessage(Items[I])
    else Result := nil;
end;

{ TConfirmMessage }

function TConfirmMessage.DefaultActionText: string;
begin
  if DefaultAction = mrNone then Result := ''
  else if DefaultAction = mrYes then Result := SYes
  else if DefaultAction = mrNo then Result := SNo
  else if DefaultAction = mrCancel then Result := SCancel
  else if DefaultAction = mrOK then Result := SOk
  else if DefaultAction = mrAbort then Result := SAbort
  else if DefaultAction = mrRetry then Result := SRetry
  else if DefaultAction = mrIgnore then Result := SIgnore
  else if DefaultAction = mrClose then Result := SClose;
end;


initialization
//  {$I UConfirmationForm.lrs}

end.

