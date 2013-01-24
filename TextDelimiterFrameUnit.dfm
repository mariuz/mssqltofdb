object TextDelimiterFrame: TTextDelimiterFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 219
  TabOrder = 0
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 136
    Height = 13
    Caption = 'Text File Characteristics'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object grpLayout: TRadioGroup
    Left = 0
    Top = 16
    Width = 313
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Data Layout'
    ItemIndex = 0
    Items.Strings = (
      '&Delimited (fields separated by tabs or commas)'
      '&Fixed width (fields aligned in columns)')
    TabOrder = 0
    OnClick = grpLayoutClick
  end
  object grpDelimiter: TRadioGroup
    Left = 0
    Top = 96
    Width = 313
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Delimiter'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      '&Tab'
      '&Space'
      'Se&micolon'
      '&Comma'
      'Das&h')
    TabOrder = 1
  end
  object chkFirstRowHasNames: TCheckBox
    Left = 0
    Top = 80
    Width = 169
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'First row has field &names.'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
end
