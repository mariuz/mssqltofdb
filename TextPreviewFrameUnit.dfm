object TextPreviewFrame: TTextPreviewFrame
  Left = 0
  Top = 0
  Width = 510
  Height = 241
  TabOrder = 0
  object btnBrowse: TButton
    Left = 433
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Browse...'
    TabOrder = 0
    OnClick = btnBrowseClick
  end
  object pnlPreview: TPanel
    Left = 0
    Top = 0
    Width = 510
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 96
      Height = 13
      Caption = 'Text File Preview'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object dlgOpenText: TOpenDialog
    DefaultExt = '*.csv'
    Filter = 'Text Files (*.csv;*.txt)|*.csv;*.tsv;*.txt|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Locate Text File Data Source'
    Left = 200
    Top = 208
  end
end
