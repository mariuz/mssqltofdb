object WelcomeFrame: TWelcomeFrame
  Left = 0
  Top = 0
  Width = 372
  Height = 302
  TabOrder = 0
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 260
    Height = 13
    Caption = 'Welcome to the MS SQL to InterBase Wizard!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 305
    Height = 41
    AutoSize = False
    Caption = 
      'This wizard will guide you through the steps required to create ' +
      'an new InterBase database from a given MS SQL database.'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 8
    Top = 72
    Width = 92
    Height = 13
    Caption = 'Click Next to begin.'
  end
  object lblLoadExisting: TLabel
    Left = 8
    Top = 120
    Width = 305
    Height = 41
    AutoSize = False
    Caption = 
      'If you have already run this wizard, you can select a script fil' +
      'e (saved by clicking on Save scripts in the last step) to reload' +
      ' all options and customizations:'
    WordWrap = True
  end
  object Bevel1: TBevel
    Left = 8
    Top = 96
    Width = 305
    Height = 18
    Shape = bsBottomLine
  end
  object lblScriptLoaded: TLabel
    Left = 8
    Top = 192
    Width = 125
    Height = 13
    Caption = 'Script loaded successfully.'
    Visible = False
  end
  object Bevel2: TBevel
    Left = 7
    Top = 211
    Width = 305
    Height = 18
    Shape = bsBottomLine
  end
  object txtScriptName: TEdit
    Left = 8
    Top = 160
    Width = 225
    Height = 21
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 240
    Top = 160
    Width = 75
    Height = 25
    Caption = '&Browse...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object btnLoadScript: TButton
    Left = 240
    Top = 192
    Width = 75
    Height = 25
    Caption = '&Load script'
    TabOrder = 2
    OnClick = btnLoadScriptClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 232
    Width = 305
    Height = 41
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'You can also use a text file as the data source.'
      'Press Schema to build a schema.ini file if you do not have one '
      'defined.')
    ReadOnly = True
    TabOrder = 3
  end
  object btnSchema: TButton
    Left = 240
    Top = 272
    Width = 75
    Height = 25
    Caption = '&Schema...'
    TabOrder = 4
    OnClick = btnSchemaClick
  end
  object dlgOpenScript: TOpenDialog
    DefaultExt = 'sql'
    Filter = 
      'SQL Files (*.sql)|*.sql|Text Files (*.txt;*.log)|*.txt;*.log|All' +
      ' Files (*.*)|*.*'
    Left = 208
    Top = 160
  end
end
