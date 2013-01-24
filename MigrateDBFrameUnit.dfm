object MigrateDBFrame: TMigrateDBFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 283
  TabOrder = 0
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 101
    Height = 13
    Caption = 'Migrate Database'
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
    Height = 25
    AutoSize = False
    Caption = 'This wizard is ready to migrate your database.'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 8
    Top = 56
    Width = 305
    Height = 33
    AutoSize = False
    Caption = 
      'Click on the Migrate button to begin the process, and then click' +
      ' on the Finish button to close this wizard.'
    WordWrap = True
  end
  object lblFeedback: TLabel
    Left = 8
    Top = 128
    Width = 58
    Height = 13
    Caption = 'lblFeedback'
    Visible = False
  end
  object Button1: TButton
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Caption = '&Migrate'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 144
    Width = 297
    Height = 137
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    Visible = False
  end
  object btnSaveScripts: TButton
    Left = 88
    Top = 96
    Width = 89
    Height = 25
    Caption = '&Save scripts...'
    TabOrder = 2
    OnClick = btnSaveScriptsClick
  end
  object btnSaveReport: TButton
    Left = 184
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Save &report...'
    TabOrder = 3
    Visible = False
    OnClick = btnSaveReportClick
  end
  object dlgSaveScript: TSaveDialog
    DefaultExt = 'sql'
    Filter = 
      'SQL Files (*.sql)|*.sql|Text Files (*.txt;*.log)|*.txt;*.log|All' +
      ' Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 264
    Top = 96
  end
  object dlgSaveReport: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 264
    Top = 128
  end
end
