object TextFieldsFrame: TTextFieldsFrame
  Left = 0
  Top = 0
  Width = 510
  Height = 241
  TabOrder = 0
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 131
    Height = 13
    Caption = 'Schema.ini for Text File'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblFieldTypes: TLabel
    Left = 0
    Top = 208
    Width = 425
    Height = 33
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 
      'Valid data types are: Bit, Byte, Char, Currency, DateTime, Float' +
      ', Integer, Long, LongChar, Single.'
    Layout = tlCenter
    WordWrap = True
  end
  object grdFieldDefs: TStringGrid
    Left = 0
    Top = 16
    Width = 502
    Height = 193
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 4
    DefaultColWidth = 75
    DefaultRowHeight = 18
    RowCount = 24
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 0
  end
  object btnSave: TButton
    Left = 433
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Save'
    TabOrder = 1
    OnClick = btnSaveClick
  end
end
