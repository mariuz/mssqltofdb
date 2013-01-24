object SelectIBFrame: TSelectIBFrame
  Left = 0
  Top = 0
  Width = 328
  Height = 327
  TabOrder = 0
  object lblTitle: TLabel
    Left = 8
    Top = 8
    Width = 153
    Height = 13
    Caption = 'Select InterBase Database'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblSelectIB: TLabel
    Left = 8
    Top = 32
    Width = 237
    Height = 13
    Caption = 'Select the &InterBase database you want to create:'
    FocusControl = txtIB
  end
  object lblIBUserName: TLabel
    Left = 8
    Top = 120
    Width = 117
    Height = 13
    Caption = 'What is your &user name?'
  end
  object lblPassword: TLabel
    Left = 8
    Top = 160
    Width = 113
    Height = 13
    Caption = 'What is your &password?'
  end
  object Label3: TLabel
    Left = 136
    Top = 120
    Width = 166
    Height = 65
    AutoSize = False
    Caption = 
      'The default values for your user name and password are those fou' +
      'nd in the InterBase documentation.'
    WordWrap = True
  end
  object lblOutputFile: TLabel
    Left = 8
    Top = 208
    Width = 305
    Height = 33
    AutoSize = False
    Caption = 
      'Optionally, you can save all the information to a file, instead ' +
      'of creating an InterBase database.'
    WordWrap = True
  end
  object lblOutputFileName: TLabel
    Left = 8
    Top = 264
    Width = 48
    Height = 13
    Caption = '&File name:'
    FocusControl = txtOutputFile
  end
  object txtIB: TEdit
    Left = 8
    Top = 64
    Width = 225
    Height = 21
    TabOrder = 0
    Text = 'C:\BugTrack\BugTrack.gdb'
  end
  object btnBrowseIB: TButton
    Left = 240
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnBrowseIBClick
  end
  object chkDeleteExisting: TCheckBox
    Left = 8
    Top = 96
    Width = 225
    Height = 17
    Caption = 'Delete the existing database (if any).'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object txtIBUserName: TEdit
    Left = 8
    Top = 136
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'sysdba'
  end
  object txtIBPassword: TEdit
    Left = 8
    Top = 176
    Width = 121
    Height = 21
    PasswordChar = 'X'
    TabOrder = 4
    Text = 'masterkey'
  end
  object chkOutputFile: TCheckBox
    Left = 8
    Top = 240
    Width = 241
    Height = 17
    Caption = '&Yes, I want to migrate the database to a file.'
    TabOrder = 5
    OnClick = chkOutputFileClick
  end
  object txtOutputFile: TEdit
    Left = 64
    Top = 264
    Width = 169
    Height = 21
    Enabled = False
    TabOrder = 6
    Text = 'C:\BugTrack\BugTrack.sql'
  end
  object btnBrowseOutputFile: TButton
    Left = 240
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Browse...'
    Enabled = False
    TabOrder = 7
    OnClick = btnBrowseOutputFileClick
  end
  object chkUseExisting: TCheckBox
    Left = 208
    Top = 96
    Width = 97
    Height = 17
    Caption = '&Use existing.'
    TabOrder = 8
  end
  object dlgSelectIB: TSaveDialog
    DefaultExt = 'gdb'
    Filter = 'InterBase Files (*.gdb)|*.gdb|All Files (*.*)|*.*'
    Title = 'Select InterBase Database'
    Left = 288
    Top = 32
  end
  object dlgSaveOutputFile: TSaveDialog
    DefaultExt = 'sql'
    Filter = 
      'Script Files (*.sql)|*.sql|Text Files (*.txt)|*.txt|All Files (*' +
      '.*)|*.*'
    Title = 'Select Output File'
    Left = 240
    Top = 288
  end
end
