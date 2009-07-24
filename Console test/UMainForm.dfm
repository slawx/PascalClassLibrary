object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 445
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  DesignSize = (
    651
    445)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 645
    Height = 410
    Margins.Bottom = 32
    Align = alClient
    ExplicitLeft = 8
    ExplicitTop = -2
    ExplicitWidth = 566
    ExplicitHeight = 464
  end
  object Button1: TButton
    Left = 8
    Top = 418
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
    OnKeyPress = FormKeyPress
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 312
    Top = 88
  end
end
