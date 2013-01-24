{*****************************************************************************}
{                                                                             }
{ Welcome Frame                                                               }
{ Provides controls to present a welcome screen and restart a saved           }
{ migration process.                                                          }
{                                                                             }
{*****************************************************************************}

unit WelcomeFrameUnit;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, TextFormUnit;

type
  TWelcomeFrame = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblLoadExisting: TLabel;
    txtScriptName: TEdit;
    btnBrowse: TButton;
    btnLoadScript: TButton;
    dlgOpenScript: TOpenDialog;
    Bevel1: TBevel;
    lblScriptLoaded: TLabel;
    Bevel2: TBevel;
    Memo1: TMemo;
    btnSchema: TButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnLoadScriptClick(Sender: TObject);
    procedure btnSchemaClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses WizFlowUnit;

{$R *.DFM}

{ TWelcomeFrame }

procedure TWelcomeFrame.btnBrowseClick(Sender: TObject);
begin
  if dlgOpenScript.Execute then begin
    txtScriptName.Text := dlgOpenScript.FileName;
    btnLoadScript.Click;
  end;
end;

procedure TWelcomeFrame.btnLoadScriptClick(Sender: TObject);
begin
  Flow.LoadFromScript(txtScriptName.Text);
  lblScriptLoaded.Visible := True;
end;

procedure TWelcomeFrame.btnSchemaClick(Sender: TObject);
var
  TextForm: TTextForm;
begin
  TextForm := TTextForm.Create(Application);
  try
    if TextForm.ShowModal <> mrOk then Exit;
  finally
    TextForm.Free;
  end;
end;

end.
