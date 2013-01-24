unit MigrateDBFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TMigrateDBFrame = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    lblFeedback: TLabel;
    Memo1: TMemo;
    btnSaveScripts: TButton;
    dlgSaveScript: TSaveDialog;
    btnSaveReport: TButton;
    dlgSaveReport: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure btnSaveScriptsClick(Sender: TObject);
    procedure btnSaveReportClick(Sender: TObject);
  private
    FThread: TThread;
    procedure DisableUI;
    procedure EnableUI;
    procedure OnThreadError(Sender: TObject; Value: string);
    procedure OnThreadExecute(Sender: TObject);
    procedure OnThreadFinish(Sender: TObject);
    procedure WriteBatchFile(const ScriptFileName, BatchFileName: string);
  public
    procedure Feedback(const Text: string);
    procedure ShowErrors(const Errors: TStrings);
  end;

implementation

uses
  ActiveX, ConvertInfoUnit, Registry, TypInfo, WizFlowUnit;

const
  sLocalMachinePathToWinRar =
    '\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\WinRAR.exe';

{$R *.DFM}

procedure EnableControls(Controls: array of TControl; Value: Boolean = True);
var
  i: Integer;
begin
  for i := Low(Controls) to High(Controls) do
    Controls[i].Enabled := Value;
end;

{-TMigrationThread-------------------------------------------------------------}
type
  TStringEvent = procedure (Sender: TObject; Value: string) of object;
  TMigrationThread = class(TThread)
  private
    FErrorFrame: TMigrateDBFrame;
    FErrorList: TStringList;
    FErrorDescription: string;
    FOnError: TStringEvent;
    FOnExecute: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    procedure DoError;
    procedure DoFinish;
    procedure DoShowErrors;
  public
    destructor Destroy; override;
    procedure Execute; override;
    procedure SynchronizeErrors(Frame: TMigrateDBFrame; Errors: TStrings);
    property OnError: TStringEvent read FOnError write FOnError;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

destructor TMigrationThread.Destroy;
begin
  if Assigned(FErrorList) then
    FErrorList.Free;
  inherited;
end;

procedure TMigrationThread.DoError;
begin
  if Assigned(FOnError) then
    FOnError(Self, FErrorDescription);
end;

procedure TMigrationThread.DoFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

procedure TMigrationThread.DoShowErrors;
begin
  FErrorFrame.ShowErrors(FErrorList);
  FreeAndNil(FErrorList);
end;

procedure TMigrationThread.Execute;
begin
  try
    if Assigned(FOnExecute) then
      FOnExecute(Self);
  except
    on E: Exception do begin
      FErrorDescription := E.ClassName + ': ' + E.Message;
      Synchronize(DoError);
    end;
  end;
  Synchronize(DoFinish);
end;

procedure TMigrationThread.SynchronizeErrors(Frame: TMigrateDBFrame;
  Errors: TStrings);
begin
  FErrorFrame := Frame;
  if not Assigned(FErrorList) then
    FErrorList := TStringList.Create;
  FErrorList.Assign(Errors);
  Synchronize(DoShowErrors);
end;

{------------------------------------------------------------------------------}
procedure TMigrateDBFrame.Button1Click(Sender: TObject);
var
  Thread: TMigrationThread;
begin
  Thread := TMigrationThread.Create(True);
  try
    FThread := Thread;
    Thread.FreeOnTerminate := True;
    Thread.OnError := OnThreadError;
    Thread.OnExecute := OnThreadExecute;
    Thread.OnFinish := OnThreadFinish;
    DisableUI;
    Thread.Resume;
  except
    EnableUI;
    Thread.Free;
    raise;
  end;
end;

procedure TMigrateDBFrame.Feedback(const Text: string);
begin
  lblFeedback.Caption := Text;
  lblFeedback.Visible := True;
  Repaint;
end;

procedure TMigrateDBFrame.ShowErrors(const Errors: TStrings);
begin
  if GetCurrentThreadId = FThread.ThreadID then begin
    (FThread as TMigrationThread).SynchronizeErrors(Self, Errors);
    Exit;
  end;
  if Errors.Count = 0 then
    Feedback('Successful conversion.')
  else begin
    Feedback('Errors reported:');
    Memo1.Lines := Errors;
    Memo1.Visible := True;
    btnSaveReport.Visible := True;
  end;
end;

function QuoteAsNecessary(const Value: string): string;
begin
  if Pos(' ', Value) = 0 then
    Result := Value else
    Result := AnsiQuotedStr(Value, '"');
end;

function GetWinRarPath: string;
var
  R: TRegistry;
begin
  R := TRegistry.Create(KEY_READ);
  try
    R.RootKey := HKEY_LOCAL_MACHINE;
    if R.OpenKeyReadOnly(sLocalMachinePathToWinRar) then
      Result := R.ReadString('Path') else
      Result := '';
  finally
    R.Free;
  end;
end;

function GetWinRarExe: string;
var
  R: TRegistry;
begin
  R := TRegistry.Create(KEY_READ);
  try
    R.RootKey := HKEY_LOCAL_MACHINE;
    if R.OpenKeyReadOnly(sLocalMachinePathToWinRar) then
      Result := R.ReadString('') else
      Result := '';
  finally
    R.Free;
  end;
end;

procedure TMigrateDBFrame.btnSaveScriptsClick(Sender: TObject);
var
  BatchFileName: string;
  Msg: string;
begin
  if not dlgSaveScript.Execute then
    Exit;
  Flow.SaveToScript(dlgSaveScript.FileName);
  BatchFileName := ChangeFileExt(dlgSaveScript.FileName, '.bat');
  Msg := Format('Would you like to create a schedulable file?'#13#10 +
    'Batch file name: %s', [BatchFileName]);
  if MessageDlg(Msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    WriteBatchFile(dlgSaveScript.FileName, BatchFileName);
    MessageDlg('Schedulable file created.'#13#10 +
      'Inspect ' + BatchFileName + ' for details and advanced options.',
      mtInformation, [mbOk], 0);
  end;
end;

procedure TMigrateDBFrame.btnSaveReportClick(Sender: TObject);
begin
  if not dlgSaveReport.Execute then
    Exit;
  Memo1.Lines.SaveToFile(dlgSaveReport.FileName);
end;

procedure TMigrateDBFrame.DisableUI;
begin
  EnableControls([Button1, btnSaveScripts, btnSaveReport], False);
end;

procedure TMigrateDBFrame.EnableUI;
begin
  EnableControls([Button1, btnSaveScripts, btnSaveReport], True);
end;

procedure TMigrateDBFrame.OnThreadExecute(Sender: TObject);
var
  OldScreenCursor: TCursor;
begin
  OldScreenCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    CoInitialize(nil);
    Flow.DisableUI;
    Flow.ConvertInfo.Execute;
  finally
    CoUninitialize;
    Flow.EnableUI;
    Screen.Cursor := OldScreenCursor;
  end;
end;

procedure TMigrateDBFrame.OnThreadFinish(Sender: TObject);
begin
  EnableUI;
end;

procedure TMigrateDBFrame.OnThreadError(Sender: TObject; Value: string);
var
  S: string;
begin
  S := 'The following error has occurred.'#13#10 + Value;
  MessageDlg(S, mtError, [mbOk], 0);
  EnableUI;
end;

procedure TMigrateDBFrame.WriteBatchFile(const ScriptFileName,
  BatchFileName: string);
var
  F: TextFile;
  DestFileName: string;
  DestRarFileName: string;
  SubmitFileName: string;
  WinRarPath: string;
  procedure W(const PartialLine: string);
  begin
    Write(F, PartialLine);
  end;
  procedure WL(const Line: string);
  begin
    Writeln(F, Line);
  end;
  procedure RemWL(const Line: string);
  begin
    Writeln(F, 'rem ' + Line);
  end;
  procedure WLFmt(const Fmt: string; const Args: array of const);
  begin
    WL(Format(Fmt, Args));
  end;
begin
  SubmitFileName := ChangeFileExt(BatchFileName, '_submit.vbs');
  AssignFile(F, BatchFileName);
  Rewrite(F);
  try
    DestFileName := Flow.ConvertInfo.IBFileName;
    WL('@echo off');
    RemWL('- Schedulable migration batch file.');
    RemWL('- Generated by sql2gdb - ' + DateTimeToStr(Now));
    WL('echo Migrating database...');
    WL(QuoteAsNecessary(ParamStr(0)) + ' ' + QuoteAsNecessary(ScriptFileName));
    WL('echo Database migrated.');
    { Write the compression section. }
    WinRarPath := GetWinRarPath;
    if WinRarPath <> '' then begin
      WL('');
      RemWL('- Uncomment the following lines to compress');
      RemWL('- the migrated database.');
      RemWL('SET PATH=' + QuoteAsNecessary(WinRarPath) + ';%PATH%');
      RemWL(ExtractFileDrive(DestFileName));
      RemWL('cd ' + QuoteAsNecessary(ExtractFilePath(DestFileName)));
      W('rem rar a ');
      W(ChangeFileExt(ExtractFileName(DestFileName), '.rar'));
      WL(' ' + ExtractFileName(DestFileName));
    end else begin
      WL('');
      RemWL('- You might want to compress the migrated database');
      RemWL('- before any further manipulation.');
    end;
    { Write the email submission section. }
    WL('');
    RemWL('- Uncomment the following line');
    RemWL('- to send the compressed file by email.');
    RemWL('- Remember to modify the destination address in the');
    RemWL('- submission script file.');
    RemWL('wscript ' + QuoteAsNecessary(SubmitFileName));
    { Write the pause section. }
    WL('');
    RemWL('- Uncomment the following line to view results interactively');
    RemWL('pause');
  finally
    CloseFile(F);
  end;
  { Write the Oulook submission file. }
  DestRarFileName := ChangeFileExt(DestFileName, '.rar');
  AssignFile(F, SubmitFileName);
  Rewrite(F);
  try
    WL('Option Explicit');
    WL('');
    WLFmt('Const strAttachmentSource = "%s"', [DestRarFileName]);
    WLFmt('Const strDatabaseName = "%s"', [ExtractFileName(DestRarFileName)]);
    WL(''' Select these other constants to attach the original migrated database');
    WLFmt('Const strAttachmentSource = "%s"', [DestFileName]);
    WLFmt('Const strDatabaseName = "%s"', [ExtractFileName(DestFileName)]);
    WL('Const strDestinationAddress = "someone@somewhere.com"');
    WL('Const olMailItem = 0');
    WL('');
    WL(''' Create the message subject and body');
    WL('Dim strSubject, strBody');
    WL('strSubject = "Migrated database " & strDatabaseName & " [" & CStr(Date) & "]"');
    WL('strBody = "<div style=''font-size: 9pt; font-family: Arial;''>" _');
    WL('  & "<p><b>Database <b>" & strDatabaseName & " migrated successfully</b></p>" _');
    WL('  & "</div>"');
    WL('');
    WL(''' Create an Outlook instance.');
    WL('Dim objOutlook, objMail');
    WL('Set objOutlook = CreateObject("Outlook.Application")');
    WL('');
    WL(''' Create an email object and send it');
    WL('Set objMail = objOutlook.CreateItem(olMailItem)');
    WL('objMail.To = strDestinationAddress');
    WL('objMail.Subject = strSubject');
    WL('objMail.HTMLBody = strBody');
    WL('objMail.Attachments.Add strAttachmentSource');
    WL('objMail.Send');
    WL('');
    WL(''' Free acquired resources');
    WL('Set objMail = Nothing');
    WL('Set objOutlook = Nothing');
  finally
    CloseFile(F);
  end;
end;

end.
