object DefaultValuesForm: TDefaultValuesForm
  Left = 192
  Top = 107
  Width = 457
  Height = 365
  Anchors = [akLeft, akRight, akBottom]
  BorderIcons = [biSystemMenu]
  Caption = 'Default Values Form'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 291
    Width = 433
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsBottomLine
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 425
    Height = 153
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      
        'Use this form to specify the default values to be used when NULL' +
        ' values are found.'
      ''
      
        'You can specify the default values in any of the following synta' +
        'xes:'
      '1. TableName.FieldName=ConstantValue'
      '2. *.FieldName=ConstantValue'
      '3. TableName.*=ConstantValue'
      '4. FieldName=ConstantValue'
      ''
      
        'The stars are used as a wildcard, and match and table or field n' +
        'ame. The fourth syntax is '
      
        'a shorthand for the second syntax. You can save the values to an' +
        ' external files, or save it '
      'within the script at the end of this wizard.')
    ReadOnly = True
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 288
    Top = 307
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 368
    Top = 307
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkCancel
  end
  object grdDefaultValues: TStringGrid
    Left = 8
    Top = 168
    Width = 425
    Height = 124
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 24
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 1
    OnKeyUp = grdDefaultValuesKeyUp
    ColWidths = (
      185
      222)
  end
  object btnSaveToFile: TButton
    Left = 8
    Top = 304
    Width = 89
    Height = 25
    Caption = '&Save to file...'
    TabOrder = 4
    OnClick = btnSaveToFileClick
  end
  object btnLoadFromFile: TButton
    Left = 96
    Top = 304
    Width = 89
    Height = 25
    Caption = '&Load from file...'
    TabOrder = 5
    OnClick = btnLoadFromFileClick
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 200
    Top = 304
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 232
    Top = 304
  end
end
