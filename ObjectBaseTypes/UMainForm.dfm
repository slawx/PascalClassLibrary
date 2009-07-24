object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 317
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Test TBoolean'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 120
    Top = 8
    Width = 353
    Height = 289
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Top = 39
    Width = 89
    Height = 25
    Caption = 'Test TByte'
    TabOrder = 2
  end
  object Button3: TButton
    Left = 8
    Top = 70
    Width = 89
    Height = 25
    Caption = 'Test TInteger'
    TabOrder = 3
  end
  object Button4: TButton
    Left = 8
    Top = 101
    Width = 89
    Height = 25
    Caption = 'Test TString'
    TabOrder = 4
    OnClick = Button4Click
  end
end
