unit UExceptionForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CustomLineInfo, ComCtrls, ExtCtrls, UStackTrace, UExceptionLogger;

type

  { TMainForm }

  { TExceptionForm }

  TExceptionForm = class(TForm)
    ButtonDetails: TButton;
    ButtonClose: TButton;
    ButtonKill: TButton;
    CheckBoxIgnore: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    LabelMessage: TLabel;
    ListView1: TListView;
    MemoExceptionInfo: TMemo;
    PageControl1: TPageControl;
    PanelBasic: TPanel;
    PanelDescription: TPanel;
    PanelButtons: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonDetailsClick(Sender: TObject);
    procedure ButtonKillClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  public
    Logger: TExceptionLogger;
    procedure LoadStackTraceToListView(StackTrace: TStackTrace);
  end;

var
  ExceptionForm: TExceptionForm;

implementation

{$R *.lfm}

procedure TExceptionForm.FormShow(Sender: TObject);
begin
  Height := PanelBasic.Height + PanelButtons.Height;
  PageControl1.ActivePageIndex := 0;
  CheckBoxIgnore.Checked := False;
end;

procedure TExceptionForm.Image1Click(Sender: TObject);
begin

end;

procedure TExceptionForm.FormCreate(Sender: TObject);
begin

end;

procedure TExceptionForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TExceptionForm.ButtonDetailsClick(Sender: TObject);
begin
  if PanelDescription.Height = 0 then
    Height := PanelBasic.Height + PanelButtons.Height + 200
    else Height := PanelBasic.Height + PanelButtons.Height;
  Application.ProcessMessages;
  if MemoExceptionInfo.Text = '' then Logger.LoadDetails;
end;

procedure TExceptionForm.ButtonKillClick(Sender: TObject);
begin
  //Halt;
  Application.Terminate;
end;

procedure TExceptionForm.FormDestroy(Sender: TObject);
begin
end;

procedure TExceptionForm.LoadStackTraceToListView(StackTrace: TStackTrace);
var
  I: Integer;
  NewItem: TListItem;
begin
  with ListView1, Items do
  try
    BeginUpdate;
    Clear;
    for I := 0 to StackTrace.Count - 1 do
    with TStackFrameInfo(StackTrace[I]) do begin
      NewItem := Add;
      with NewItem do begin
        Caption := IntToStr(Index);
        SubItems.Add(IntToHex(Address, 8));
        SubItems.Add(IntToStr(LineNumber));
        SubItems.Add(FunctionClassName);
        SubItems.Add(FunctionName);
        SubItems.Add(Source);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

initialization

ExceptionForm := TExceptionForm.Create(nil);

finalization

FreeAndNil(ExceptionForm);

end.

