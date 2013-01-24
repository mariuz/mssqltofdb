object CustomizeMigrationFrame: TCustomizeMigrationFrame
  Left = 0
  Top = 0
  Width = 331
  Height = 315
  TabOrder = 0
  object lblTitle: TLabel
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
  object lblHeader: TLabel
    Left = 8
    Top = 32
    Width = 313
    Height = 33
    AutoSize = False
    Caption = 
      'This wizard will generate scripts to create database objects, an' +
      'd then execute them.'
    WordWrap = True
  end
  object lblScriptingFeedback: TLabel
    Left = 8
    Top = 264
    Width = 305
    Height = 41
    AutoSize = False
  end
  object grpCustomizations: TGroupBox
    Left = 8
    Top = 64
    Width = 321
    Height = 193
    Caption = 'Optional Customizations'
    TabOrder = 0
    object lblNameMapMethod: TLabel
      Left = 8
      Top = 112
      Width = 92
      Height = 13
      Caption = 'Name map method:'
      FocusControl = cmbMapMethod
    end
    object lblCharSet: TLabel
      Left = 8
      Top = 136
      Width = 68
      Height = 13
      Caption = 'Cha&racter Set:'
      FocusControl = cmbCharSet
    end
    object btnSelectTables: TButton
      Left = 8
      Top = 160
      Width = 89
      Height = 25
      Caption = '&Select Tables...'
      TabOrder = 1
      OnClick = btnSelectTablesClick
    end
    object cmbMapMethod: TComboBox
      Left = 104
      Top = 112
      Width = 201
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'Use quotes (dialect 3 only)'
        'Use underscores'
        'Remove whitespace')
    end
    object cmbCharSet: TComboBox
      Left = 104
      Top = 136
      Width = 201
      Height = 21
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'ASCII'
        'BIG_5'
        'CYRL'
        'DOS437'
        'DOS850'
        'DOS852'
        'DOS857'
        'DOS860'
        'DOS861'
        'DOS863'
        'DOS865'
        'EUCJ_0208'
        'GB_2312'
        'ISO8859_1'
        'KSC_5601'
        'NEXT'
        'NONE'
        'OCTETS'
        'SJIS_0208'
        'UNICODE_FSS'
        'WIN1250'
        'WIN1251'
        'WIN1252'
        'WIN1253'
        'WIN1254')
    end
    object btnSelectLibraries: TButton
      Left = 96
      Top = 160
      Width = 121
      Height = 25
      Caption = 'Select UDF &Libraries...'
      TabOrder = 3
      OnClick = btnSelectLibrariesClick
    end
    object btnSelectDefaults: TButton
      Left = 216
      Top = 160
      Width = 97
      Height = 25
      Caption = 'Select De&faults...'
      TabOrder = 4
      OnClick = btnSelectDefaultsClick
    end
    object lstOptions: TCheckListBox
      Left = 8
      Top = 16
      Width = 297
      Height = 89
      OnClickCheck = lstOptionsClickCheck
      ItemHeight = 13
      Items.Strings = (
        'Use dialect 3.'
        'Migrate autonumeric fields.'
        'Skip metadata generation.'
        'Skip trigger generation.'
        'Create GUID domain.'
        'Interrupt on errors.')
      TabOrder = 5
    end
  end
end
