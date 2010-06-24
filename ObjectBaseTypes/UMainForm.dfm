object MainForm: TMainForm
  Left = 451
  Height = 317
  Top = 192
  Width = 484
  Caption = 'MainForm'
  ClientHeight = 317
  ClientWidth = 484
  Font.Height = -11
  Font.Name = 'Tahoma'
  LCLVersion = '0.9.29'
  object Button1: TButton
    Left = 8
    Height = 25
    Top = 8
    Width = 89
    Caption = 'Test TBoolean'
    OnClick = Button1Click
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 120
    Height = 289
    Top = 8
    Width = 353
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Height = 25
    Top = 39
    Width = 89
    Caption = 'Test TByte'
    TabOrder = 2
  end
  object Button3: TButton
    Left = 8
    Height = 25
    Top = 70
    Width = 89
    Caption = 'Test TInteger'
    TabOrder = 3
  end
  object Button4: TButton
    Left = 8
    Height = 25
    Top = 101
    Width = 89
    Caption = 'Test TString'
    OnClick = Button4Click
    TabOrder = 4
  end
end
