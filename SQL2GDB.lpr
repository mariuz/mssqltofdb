program SQL2GDB;

uses
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  WelcomeFrameUnit in 'WelcomeFrameUnit.pas' {WelcomeFrame: TFrame},
  SelectSourceFrameUnit in 'SelectSourceFrameUnit.pas' {SelectSourceFrame: TFrame},
  SelectIBFrameUnit in 'SelectIBFrameUnit.pas' {SelectIBFrame: TFrame},
  CustomizeMigrationFrameUnit in 'CustomizeMigrationFrameUnit.pas' {CustomizeMigrationFrame: TFrame},
  MigrateDBFrameUnit in 'MigrateDBFrameUnit.pas' {MigrateDBFrame: TFrame},
  WizFlowUnit in 'WizFlowUnit.pas',
  ADOX_TLB in 'ADOX_TLB.pas',
  ADODB_TLB in 'ADODB_TLB.pas',
  MigrationUnit in 'MigrationUnit.pas',
  Base64Unit in 'Base64Unit.pas',
  DragDropUnit in 'DragDropUnit.pas',
  CustomizeScriptsFrameUnit in 'CustomizeScriptsFrameUnit.pas' {CustomizeScriptsFrame: TFrame},
  IBUpdateUnit in 'IBUpdateUnit.pas',
  IBCheckUpdatesUnit in 'IBCheckUpdatesUnit.pas',
  ConvertInfoUnit in 'ConvertInfoUnit.pas',
  SQL2GDBConstsUnit in 'SQL2GDBConstsUnit.pas',
  AdoUtils in 'AdoUtils.pas',
  SQL2GDBCmdLineUnit in 'SQL2GDBCmdLineUnit.pas',
  MLRFileIteratorUnit in 'MLRFileIteratorUnit.pas',
  DefaultValuesFormUnit in 'DefaultValuesFormUnit.pas' {DefaultValuesForm},
  TextPreviewFrameUnit in 'TextPreviewFrameUnit.pas' {TextPreviewFrame: TFrame},
  TextFieldsFrameUnit in 'TextFieldsFrameUnit.pas' {TextFieldsFrame: TFrame},
  TextFormUnit in 'TextFormUnit.pas' {TextForm},
  TextDelimiterFrameUnit in 'TextDelimiterFrameUnit.pas' {TextDelimiterFrame: TFrame},
  TextModelUnit in 'TextModelUnit.pas',
  MLRTextColEditorUnit in 'MLRTextColEditorUnit.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'MS SQL To InterBase Wizard';
  if not RunFromCommandLine then begin
    Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  end;
end.
