{*****************************************************************************}
{                                                                             }
{ SQL2GDB Project                                                             }
{ Main Application Form                                                       }
{                                                                             }
{*****************************************************************************}

unit MainFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, WizFlowUnit, ExtCtrls, Db, AppEvnts;

type
  TMainForm = class(TForm)
    btnBack: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    Bevel1: TBevel;
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    Image1: TImage;
    Bevel2: TBevel;
    AppEvents: TApplicationEvents;
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure AppEventsException(Sender: TObject; E: Exception);
    procedure Image1Click(Sender: TObject);
  private
    FUpdatesChecked: Boolean;
    function GetLastUpdateTime: TDateTime;
    procedure CheckUpdates(const Silent: Boolean);
  public
  end;

var
  MainForm: TMainForm;

implementation

uses
  WelcomeFrameUnit,  SelectIBFrameUnit,
  CustomizeMigrationFrameUnit, MigrateDBFrameUnit, IBCheckUpdatesUnit,
  IBUpdateUnit;

const
  SSQL2GDBTopicName = 'SQL2GDB';
  LastUpdateTime = '2002.06.28.13.35';

{$R *.DFM}

procedure TMainForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  btnCancel.Top := ClientHeight - 8 - btnCancel.Height;
  btnCancel.Left := ClientWidth - 8 - btnCancel.Width;

  btnNext.Top := btnCancel.Top;
  btnNext.Left := btnCancel.Left - 8 - btnNext.Width;

  btnBack.Top := btnCancel.Top;
  btnBack.Left := btnNext.Left - btnBack.Width;

  Bevel1.Top := btnCancel.Top - 8 - Bevel1.Height;
  Bevel1.Left := 8;
  Bevel1.Width := ClientWidth - 16;

  PaintBox1.Top := 8;
  PaintBox1.Left := 8;
  PaintBox1.Height := Bevel1.Top - 8 - PaintBox1.Top;
  PaintBox1.Width := Panel1.Left - PaintBox1.Left - 16;

  Panel1.Height := PaintBox1.Height;

  Application.Title := Caption;

  Flow := TWizardFlow.Create(Panel1, btnBack, btnNext, btnCancel);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Flow.Free;
end;

procedure TMainForm.btnNextClick(Sender: TObject);
begin
  Flow.Next;
end;

procedure TMainForm.btnBackClick(Sender: TObject);
begin
  Flow.Back;
end;

procedure TMainForm.PaintBox1Paint(Sender: TObject);
var
  C: TCanvas;
begin
  C := PaintBox1.Canvas;
//  C.Brush.Color := clGreen;
  C.FillRect(PaintBox1.ClientRect);
  C.Font.Name := 'Tahoma';
  C.Font.Color := clBlack; //$80C0F0;
  C.Font.Size := 12;
  C.Font.Style := [fsBold];
  C.TextOut(8, 14, 'InterBase');
  C.Font.Size := 8;
  C.Font.Style := [];
  C.TextOut(18, 34, 'The Open Source Database');
  C.TextOut(8, PaintBox1.Height - 34, 'Click above for updates');
end;

type
  TUpdateCheckerThread = class(TThread)
  private
    FLastUpdateTime: TDateTime;
  public
    procedure Execute; override;
    property LastUpdateTime: TDateTime read FLastUpdateTime write FLastUpdateTime;
  end;

procedure TUpdateCheckerThread.Execute;
begin
  CheckIBUpdates(cmSilent, SSQL2GDBTopicName, FLastUpdateTime);
end;

procedure TMainForm.CheckUpdates(const Silent: Boolean);
var
  Thread: TUpdateCheckerThread;
begin
  if Silent then begin
    Thread := TUpdateCheckerThread.Create(True);
    Thread.FreeOnTerminate := True;
    Thread.LastUpdateTime := GetLastUpdateTime;
    Thread.Resume;
  end else
    CheckIBUpdates(cmInteractive, SSQL2GDBTopicName, GetLastUpdateTime);
end;

function TMainForm.GetLastUpdateTime: TDateTime;
begin
  Result := GetStdDate(LastUpdateTime);
end;

procedure TMainForm.AppEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if not FUpdatesChecked then begin
    FUpdatesChecked := True;
    CheckUpdates(True);
    Done := True;
  end;
end;

procedure TMainForm.AppEventsException(Sender: TObject; E: Exception);
begin
  Application.ShowException(E);
end;

procedure TMainForm.Image1Click(Sender: TObject);
begin
  CheckUpdates(False);
end;

end.
