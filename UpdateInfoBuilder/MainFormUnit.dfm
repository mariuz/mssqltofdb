object MainForm: TMainForm
  Left = 192
  Top = 107
  Width = 432
  Height = 464
  Caption = 'Update Information Builder'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDocument: TPanel
    Left = 0
    Top = 0
    Width = 424
    Height = 65
    Align = alTop
    TabOrder = 0
    object lblMajorVersion: TLabel
      Left = 8
      Top = 8
      Width = 69
      Height = 13
      Caption = 'Ma&jor Version:'
      FocusControl = txtMajorVersion
    end
    object Label2: TLabel
      Left = 216
      Top = 8
      Width = 68
      Height = 13
      Caption = 'M&inor Version:'
      FocusControl = txtMinorVersion
    end
    object Label3: TLabel
      Left = 8
      Top = 32
      Width = 62
      Height = 13
      Caption = '&Last Update:'
      FocusControl = txtLastUpdate
    end
    object txtMajorVersion: TEdit
      Left = 88
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'txtMajorVersion'
    end
    object txtMinorVersion: TEdit
      Left = 296
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'txtMinorVersion'
    end
    object txtLastUpdate: TEdit
      Left = 88
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'txtLastUpdate'
    end
    object btnSetLastUpdate: TButton
      Left = 216
      Top = 32
      Width = 97
      Height = 25
      Action = SetLastUpdate
      TabOrder = 3
    end
  end
  object pnlTopics: TPanel
    Left = 0
    Top = 65
    Width = 424
    Height = 80
    Align = alTop
    TabOrder = 1
    object lblTopics: TLabel
      Left = 8
      Top = 8
      Width = 34
      Height = 13
      Caption = '&Topics:'
      FocusControl = lstTopics
    end
    object lstTopics: TListBox
      Left = 8
      Top = 24
      Width = 409
      Height = 49
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      PopupMenu = TopicsPopup
      TabOrder = 0
      OnClick = lstTopicsClick
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 396
    Width = 424
    Height = 41
    Align = alBottom
    TabOrder = 2
    object btnNewDocument: TButton
      Left = 8
      Top = 8
      Width = 81
      Height = 25
      Action = NewDocument
      TabOrder = 0
    end
    object btnLoadDocument: TButton
      Left = 88
      Top = 8
      Width = 105
      Height = 25
      Action = LoadDocument
      TabOrder = 1
    end
    object btnSaveDocument: TButton
      Left = 192
      Top = 8
      Width = 105
      Height = 25
      Action = SaveDocument
      TabOrder = 2
    end
  end
  object pnlTopic: TPanel
    Left = 0
    Top = 145
    Width = 424
    Height = 251
    Align = alClient
    TabOrder = 3
    object lblName: TLabel
      Left = 8
      Top = 8
      Width = 31
      Height = 13
      Caption = '&Name:'
    end
    object lblDescription: TLabel
      Left = 8
      Top = 32
      Width = 57
      Height = 13
      Caption = '&Description:'
    end
    object lblLastUpdate: TLabel
      Left = 8
      Top = 56
      Width = 62
      Height = 13
      Caption = 'L&ast Update:'
    end
    object lblBody: TLabel
      Left = 8
      Top = 80
      Width = 28
      Height = 13
      Caption = '&Body:'
    end
    object txtName: TEdit
      Left = 80
      Top = 8
      Width = 337
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'txtName'
    end
    object txtDescription: TEdit
      Left = 80
      Top = 32
      Width = 337
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'txtDescription'
    end
    object txtTopicLastUpdate: TEdit
      Left = 80
      Top = 56
      Width = 233
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = 'txtTopicLastUpdate'
    end
    object txtBody: TMemo
      Left = 8
      Top = 96
      Width = 409
      Height = 145
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      Lines.Strings = (
        'txtBody')
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object btnSetTopicUpdate: TButton
      Left = 320
      Top = 56
      Width = 97
      Height = 25
      Action = SetTopicUpdate
      Anchors = [akTop, akRight]
      TabOrder = 4
    end
  end
  object Actions: TActionList
    Left = 376
    Top = 32
    object NewDocument: TAction
      Category = 'Document'
      Caption = '&New Document'
      OnExecute = NewDocumentExecute
    end
    object LoadDocument: TAction
      Category = 'Document'
      Caption = '&Load Document...'
      OnExecute = LoadDocumentExecute
    end
    object SaveDocument: TAction
      Category = 'Document'
      Caption = '&Save Document...'
      OnExecute = SaveDocumentExecute
    end
    object AddTopic: TAction
      Category = 'Document'
      Caption = '&Add Topic'
      OnExecute = AddTopicExecute
    end
    object DeleteTopic: TAction
      Category = 'Document'
      Caption = '&Delete Topic'
      OnUpdate = SetTopicUpdateUpdate
    end
    object SetLastUpdate: TAction
      Category = 'Document'
      Caption = '&Set Last Update'
      OnExecute = SetLastUpdateExecute
    end
    object SetTopicUpdate: TAction
      Category = 'Topic'
      Caption = 'Set Last &Update'
      OnExecute = SetTopicUpdateExecute
      OnUpdate = SetTopicUpdateUpdate
    end
  end
  object TopicsPopup: TPopupMenu
    Left = 336
    Top = 97
    object AddTopic1: TMenuItem
      Action = AddTopic
    end
    object DeleteTopic1: TMenuItem
      Action = DeleteTopic
    end
  end
end
