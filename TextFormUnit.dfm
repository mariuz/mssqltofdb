object TextForm: TTextForm
  Left = 195
  Top = 137
  Width = 543
  Height = 372
  Caption = 'Text File Schema Wizard'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 272
    Top = 16
    Width = 50
    Height = 10
    Shape = bsBottomLine
  end
  object TextPanel: TPanel
    Left = 8
    Top = 50
    Width = 520
    Height = 255
    BevelOuter = bvNone
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 245
    Top = 8
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnBack: TButton
    Left = 85
    Top = 8
    Width = 75
    Height = 25
    Caption = '< &Back'
    TabOrder = 2
    OnClick = btnBackClick
  end
  object btnNext: TButton
    Left = 165
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Next >'
    TabOrder = 3
    OnClick = btnNextClick
  end
end
