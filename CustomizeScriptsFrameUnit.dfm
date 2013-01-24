object CustomizeScriptsFrame: TCustomizeScriptsFrame
  Left = 0
  Top = 0
  Width = 328
  Height = 247
  TabOrder = 0
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 114
    Height = 13
    Caption = 'Customize Migration'
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
    Height = 49
    AutoSize = False
    Caption = 
      'The wizard has generated the following scripts. You can customiz' +
      'e any of them, but if you do, be careful: parameter binding for ' +
      'INSERT statements may fail.'
    WordWrap = True
  end
  object Label5: TLabel
    Left = 8
    Top = 96
    Width = 35
    Height = 13
    Caption = '&Scripts:'
    FocusControl = cmbScripts
  end
  object cmbScripts: TComboBox
    Left = 48
    Top = 96
    Width = 273
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cmbScriptsChange
  end
  object txtSQLCommand: TMemo
    Left = 8
    Top = 120
    Width = 313
    Height = 89
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'txtSQLCommand')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    OnExit = txtSQLCommandExit
  end
  object btnClearScript: TButton
    Left = 232
    Top = 216
    Width = 91
    Height = 25
    Caption = 'Clear &this script'
    TabOrder = 2
    OnClick = btnClearScriptClick
  end
end
