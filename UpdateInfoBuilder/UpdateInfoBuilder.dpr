program UpdateInfoBuilder;

uses
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  IBUpdateUnit in '..\IBUpdateUnit.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Update Information Builder';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
