unit UMessagesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, UCDClient;

type

  { TMessagesForm }

  TMessagesForm = class(TForm)
  published
    CoolDockClient1: TCDClient;
    Memo1: TMemo;
    { private declarations }
  public
    { public declarations }
  end; 

var
  MessagesForm: TMessagesForm;

implementation

{$R *.lfm}

end.

