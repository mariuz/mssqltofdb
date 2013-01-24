object SelectSourceFrame: TSelectSourceFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 139
    Height = 13
    Caption = 'Select Source Database'
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
    Height = 33
    AutoSize = False
    Caption = 
      'Select the &source database you wish to use to create a new Inte' +
      'rBase database:'
    FocusControl = txtConnectionString
    WordWrap = True
  end
  object Label3: TLabel
    Left = 8
    Top = 192
    Width = 275
    Height = 33
    AutoSize = False
    Caption = 
      'Hint: you can drop MDB, MDW, CSV or TXT files on the text area a' +
      'bove to set them up/load their info.'
    WordWrap = True
  end
  object btnBuild: TButton
    Left = 216
    Top = 160
    Width = 75
    Height = 25
    Caption = '&Build...'
    TabOrder = 0
    OnClick = btnBuildClick
  end
  object txtConnectionString: TMemo
    Left = 8
    Top = 64
    Width = 281
    Height = 89
    Lines.Strings = (
      'Provider=SQLOLEDB.1;Password=m;'
      'Persist Security Info=True;User ID=mlruiz;'
      'Initial Catalog=Northwind;'
      'Data Source=MARCELONT')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnSaveAsDefault: TButton
    Left = 120
    Top = 160
    Width = 91
    Height = 25
    Caption = 'Save as default'
    TabOrder = 2
    OnClick = btnSaveAsDefaultClick
  end
end
