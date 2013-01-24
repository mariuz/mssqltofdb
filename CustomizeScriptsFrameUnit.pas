
{*******************************************************}
{                                                       }
{    Customize Scripts Frame                            }
{                                                       }
{    Provides controls to customize the generated       }
{    scripts in the migration process.                  }
{                                                       }
{*******************************************************}

unit CustomizeScriptsFrameUnit;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TCustomizeScriptsFrame = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    cmbScripts: TComboBox;
    txtSQLCommand: TMemo;
    btnClearScript: TButton;
    procedure cmbScriptsChange(Sender: TObject);
    procedure txtSQLCommandExit(Sender: TObject);
    procedure btnClearScriptClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure LoadActiveScript;
    procedure MapFromConvertInfo;
    procedure SaveChanges;
  end;

implementation

uses WizFlowUnit;

{$R *.DFM}

{ TCustomizeScriptsFrame }

procedure TCustomizeScriptsFrame.MapFromConvertInfo;
var
  Scripts: TStrings;
begin
  Scripts := Flow.ConvertInfo.Scripts;
  cmbScripts.Items := Scripts;
  if cmbScripts.Items.Count > 0 then begin
    cmbScripts.ItemIndex := 0;
    cmbScriptsChange(Self);
  end;
end;

procedure TCustomizeScriptsFrame.cmbScriptsChange(Sender: TObject);
begin
  LoadActiveScript;
end;

procedure TCustomizeScriptsFrame.txtSQLCommandExit(Sender: TObject);
begin
  SaveChanges;
end;

procedure TCustomizeScriptsFrame.btnClearScriptClick(Sender: TObject);
begin
  txtSQLCommand.Clear;
  SaveChanges;
end;

procedure TCustomizeScriptsFrame.SaveChanges;
var
  Index: Integer;
begin
  Index := cmbScripts.ItemIndex;
  if Index <> -1 then
    TStrings(cmbScripts.Items.Objects[Index]).Assign(txtSQLCommand.Lines);
end;

procedure TCustomizeScriptsFrame.LoadActiveScript;
var
  Index: Integer;
begin
  Index := cmbScripts.ItemIndex;
  if Index = -1 then
    txtSQLCommand.Clear
  else
    txtSQLCommand.Lines.Assign(TStrings(cmbScripts.Items.Objects[Index]));
end;

end.
