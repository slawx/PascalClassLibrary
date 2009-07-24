object CommOptionsForm: TCommOptionsForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Nastaven'#237' s'#233'riov'#233' linky'
  ClientHeight = 242
  ClientWidth = 284
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    284
    242)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 63
    Height = 13
    Caption = 'S'#233'riov'#253' port:'
  end
  object Label2: TLabel
    Left = 8
    Top = 43
    Width = 96
    Height = 13
    Caption = 'P'#345'enosov'#225' rychlost:'
  end
  object Label3: TLabel
    Left = 8
    Top = 75
    Width = 32
    Height = 13
    Caption = 'Parita:'
  end
  object Label4: TLabel
    Left = 8
    Top = 107
    Width = 60
    Height = 13
    Caption = 'Datov'#233' bity:'
  end
  object Label5: TLabel
    Left = 8
    Top = 139
    Width = 73
    Height = 13
    Caption = 'Po'#269'et stopbit'#367':'
  end
  object Label6: TLabel
    Left = 8
    Top = 171
    Width = 56
    Height = 13
    Caption = #344#237'zen'#237' toku:'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 200
    Width = 270
    Height = 3
    Anchors = [akLeft, akRight, akBottom]
    ExplicitWidth = 345
  end
  object ComboBox1: TComboBox
    Left = 112
    Top = 8
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      'COM1'
      'COM2'
      'COM3'
      'COM4'
      'COM5'
      'COM6'
      'COM7'
      'COM8'
      'COM9'
      'COM10'
      'COM11'
      'COM12'
      'COM13'
      'COM14')
  end
  object ComboBox2: TComboBox
    Left = 112
    Top = 40
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    Items.Strings = (
      '110'
      '300'
      '600'
      '1200'
      '2400'
      '4800'
      '9600'
      '14400'
      '19200'
      '38400'
      '56000'
      '57600'
      '115200'
      '128000'
      '256000')
  end
  object ComboBox3: TComboBox
    Left = 112
    Top = 72
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    Items.Strings = (
      #381#225'dn'#225
      'Lich'#225
      'Sud'#225
      'Zna'#269'ka'
      'Mezera')
  end
  object ComboBox4: TComboBox
    Left = 112
    Top = 104
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    Items.Strings = (
      '4'
      '5'
      '6'
      '7'
      '8')
  end
  object ComboBox5: TComboBox
    Left = 112
    Top = 136
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      '1'
      '1,5'
      '2')
  end
  object ComboBox6: TComboBox
    Left = 112
    Top = 168
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    Items.Strings = (
      #381#225'dn'#233
      'Xon/Xoff'
      'Hardwarov'#233)
  end
  object Button1: TButton
    Left = 120
    Top = 209
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 201
    Top = 209
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 7
    OnClick = Button2Click
  end
end
