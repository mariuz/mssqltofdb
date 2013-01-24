
{*******************************************************}
{                                                       }
{    Wizard Flow Unit                                   }
{                                                       }
{    Controls the flow between steps.                   }
{                                                       }
{*******************************************************}

unit WizFlowUnit;

interface

uses
  ADODB_TLB, ADOX_TLB, Classes, Controls, ConvertInfoUnit,
  CustomizeMigrationFrameUnit, CustomizeScriptsFrameUnit,
  Forms, MigrateDBFrameUnit, SelectIBFrameUnit, SelectSourceFrameUnit,
  StdCtrls, SysUtils, WelcomeFrameUnit;

type
  TWizardStep = (wsWelcome, wsSelectSource, wsSelectIB,
    wsCustomizeMigration, wsCustomizeScripts, wsExecute);

  TWizardFlow = class
  private
    FBackButton: TButton;
    FCancelButton: TButton;
    FConvertInfo: TConvertInfo;
    FCustomize: TCustomizeMigrationFrame;
    FCustomizeScripts: TCustomizeScriptsFrame;
    FMigrateDBFrame: TMigrateDBFrame;
    FNextButton: TButton;
    FSelectIBFrame: TSelectIBFrame;
    FSelectSourceFrame: TSelectSourceFrame;
    FWelcomeFrame: TWelcomeFrame;
    FWizardFrames: array[wsWelcome..wsExecute] of TFrame;
    FWizardStep: TWizardStep;
    function GetDefaultSourceFileName: string;
    procedure ExecutionFeedback(Sender: TObject; Text: string);
    procedure LoadDefaultSource;
    procedure FinishWizard(Sender: TObject);
    procedure ForwardFrom(const Step: TWizardStep);
    procedure SetWizardStep(const Value: TWizardStep);
    procedure ScriptingFeedback(Sender: TObject; Text: string);
    procedure ShowErrors(Sender: TObject; Strings: TStrings);
  public
    constructor Create(Parent: TWinControl;
      BackButton, NextButton, CancelButton: TButton);
    destructor Destroy; override;
    procedure Back;
    procedure DisableUI;
    procedure EnableUI;
    procedure LoadFromScript(const FileName: string);
    procedure Next;
    procedure SaveDefaultSource(Strings: TStrings);
    procedure SaveToScript(const FileName: string);

    property ConvertInfo: TConvertInfo read FConvertInfo;
    property WizardStep: TWizardStep read FWizardStep write SetWizardStep;
  end;

var
  Flow: TWizardFlow;

implementation

uses ComObj, Dialogs, MigrationUnit, SQL2GDBConstsUnit, TypInfo;

{ TWizardFlow }

procedure TWizardFlow.Back;
begin
  WizardStep := Pred(WizardStep);
end;

constructor TWizardFlow.Create(Parent: TWinControl; BackButton, NextButton,
  CancelButton: TButton);
var
  Step: TWizardStep;
begin
  FConvertInfo := TConvertInfo.Create;

  FBackButton := BackButton;
  FNextButton := NextButton;
  FCancelButton := CancelButton;
  
  FWelcomeFrame := TWelcomeFrame.Create(Parent);
  FWizardFrames[wsWelcome] := FWelcomeFrame;

  FSelectSourceFrame := TSelectSourceFrame.Create(Parent);
  FWizardFrames[wsSelectSource] := FSelectSourceFrame;

  FSelectIBFrame := TSelectIBFrame.Create(Parent);
  FWizardFrames[wsSelectIB] := FSelectIBFrame;

  FCustomize := TCustomizeMigrationFrame.Create(Parent);
  FWizardFrames[wsCustomizeMigration] := FCustomize;

  FCustomizeScripts := TCustomizeScriptsFrame.Create(Parent);
  FWizardFrames[wsCustomizeScripts] := FCustomizeScripts;

  FMigrateDBFrame := TMigrateDBFrame.Create(Parent);
  FWizardFrames[wsExecute] := FMigrateDBFrame;

  LoadDefaultSource;

  { Assign parents. }
  for Step := Low(FWizardFrames) to High(FWizardFrames) do begin
    FWizardFrames[Step].Visible := False;
    FWizardFrames[Step].Parent := Parent;
    FWizardFrames[Step].Top := 8;
    FWizardFrames[Step].Left := 8;
  end;

  FSelectSourceFrame.EnableDragDrop;
  {
  2002.04.29 - modified (might break code that relies on this default
  FCustomize.MapMethod := mmUseQuoted;
  }
  FCustomize.MapMethod := mmUnderscores;
  WizardStep := wsWelcome;

  { Setup events. }
  FConvertInfo.OnExecutionFeedback := ExecutionFeedback; 
  FConvertInfo.OnFinish := FinishWizard;
  FConvertInfo.OnShowErrors := ShowErrors;
  FConvertInfo.OnScriptingFeedback := ScriptingFeedback;
end;

destructor TWizardFlow.Destroy;
begin
  FConvertInfo.Free;
  inherited;
end;

procedure TWizardFlow.DisableUI;
begin
  FNextButton.Enabled := False;
  FBackButton.Enabled := False;
  FCancelButton.Enabled := False;
end;

procedure TWizardFlow.EnableUI;
begin
  FNextButton.Enabled := True;
  FBackButton.Enabled := True;
  FCancelButton.Enabled := True;
end;

procedure TWizardFlow.ExecutionFeedback(Sender: TObject; Text: string);
begin
  FMigrateDBFrame.Feedback(Text);
end;

procedure TWizardFlow.FinishWizard(Sender: TObject);
begin
  FNextButton.Enabled := True;
  FBackButton.Enabled := False;
  FCancelButton.Enabled := False;
end;

procedure TWizardFlow.ForwardFrom(const Step: TWizardStep);
begin
  case Step of
    wsSelectSource: begin
      if FSelectSourceFrame.ConnectionString = '' then
        raise Exception.Create('You must select a source ' +
          'database to continue');
      FConvertInfo.SourceConnectionString := FSelectSourceFrame.ConnectionString;
    end;
    wsSelectIB: begin
      if FSelectIBFrame.OutputType = otIB then begin
        if FSelectIBFrame.IBFileName = '' then
          raise Exception.Create(SInterBaseFileMissing);
        if FSelectIBFrame.DeleteExisting and FSelectIBFrame.UseExisting then
          raise Exception.Create(SCannotDeleteAndUse);
        FConvertInfo.OutputType := otIB;
        FConvertInfo.IBFileName := FSelectIBFrame.IBFileName;
        FConvertInfo.IBUserName := FSelectIBFrame.txtIBUserName.Text;
        FConvertInfo.IBPassword := FSelectIBFrame.txtIBPassword.Text;
        FConvertInfo.DeleteExisting := FSelectIBFrame.DeleteExisting;
        FConvertInfo.UseExisting := FSelectIBFrame.UseExisting;
      end;
      if FSelectIBFrame.OutputType = otText then begin
        if FSelectIBFrame.OutputFileName = '' then
          raise Exception.Create(SOutputFileMissing);
        FConvertInfo.OutputType := otText;
        FConvertInfo.OutputFileName := FSelectIBFrame.OutputFileName;
      end;
    end;
    wsCustomizeMigration: begin
      if (FCustomize.MapMethod = mmUseQuoted) and
         (not FCustomize.UseDialect3) then
        raise Exception.Create(SQuotesRequireDialect3);
      if (FCustomize.SkipMetadata and (not FCustomize.SkipTriggers)) then begin
        FCustomize.SkipTriggers := True;
        raise Exception.Create(STriggerSkippingImplied);
      end;
      if FCustomize.Tables.Count > 0 then
        FConvertInfo.RestrictToTables := FCustomize.Tables;
      FConvertInfo.DefaultValues := FCustomize.DefaultValues;
      FConvertInfo.Libraries := FCustomize.Libraries;
      FConvertInfo.InterruptOnErrors := FCustomize.InterruptOnErrors;
      FConvertInfo.MapMethods := [FCustomize.MapMethod];
      FConvertInfo.CharSet := FCustomize.CharSet;
      FConvertInfo.CreateGUIDDomain := FCustomize.CreateGUIDDomain;
      FConvertInfo.MigrateAutoNumber := FCustomize.MigrateAutoNumber;
      FConvertInfo.SkipMetadata := FCustomize.SkipMetadata;
      FConvertInfo.SkipTriggers := FCustomize.SkipTriggers;
      FConvertInfo.UseDialect3 := FCustomize.UseDialect3;
      if (FConvertInfo.Scripts.Count = 0) or
         (MessageDlg(SConfirmRecreateScripts,
            mtWarning, [mbYes, mbNo], 0) = mrYes) then begin
        Screen.Cursor := crHourGlass;
        try
          FCustomize.StartFeedback;
          FConvertInfo.GenerateScripts;
        finally
          FCustomize.StopFeedback;
          Screen.Cursor := crDefault;
        end;
      end;
      FCustomizeScripts.MapFromConvertInfo;
    end;
    wsCustomizeScripts: begin
      FCustomizeScripts.SaveChanges;
    end;
  end;
end;

function TWizardFlow.GetDefaultSourceFileName: string;
begin
  Result := ExtractFilePath(Application.ExeName) + 'source.txt';
end;

procedure TWizardFlow.LoadDefaultSource;
var
  FileName: string;
begin
  FileName := GetDefaultSourceFileName;
  if not FileExists(FileName) then Exit;
  FSelectSourceFrame.txtConnectionString.Lines.LoadFromFile(FileName);
end;

procedure TWizardFlow.LoadFromScript(const FileName: string);
var
  Info: TConvertInfo;
begin
  Info := FConvertInfo;
  Info.LoadFromScript(FileName);

  { Set all frame controls. }
  FCustomize.CharSet := Info.CharSet;
  FCustomize.CreateGUIDDomain := Info.CreateGUIDDomain;
  FCustomize.DefaultValues := Info.DefaultValues;
  FCustomize.InterruptOnErrors := Info.InterruptOnErrors;
  FCustomize.Libraries := Info.Libraries;
  FCustomize.MapMethod := FirstMapMethod(Info.MapMethods);
  FCustomize.MigrateAutoNumber := Info.MigrateAutoNumber;
  FCustomize.SkipMetadata := Info.SkipMetadata;
  FCustomize.Tables := Info.RestrictToTables;
  FCustomize.UseDialect3 := Info.UseDialect3;

  FCustomizeScripts.MapFromConvertInfo;
  FSelectIBFrame.DeleteExisting := Info.DeleteExisting;
  FSelectIBFrame.IBFileName := Info.IBFileName;
  FSelectIBFrame.OutputFileName := Info.OutputFileName;
  FSelectIBFrame.OutputType := Info.OutputType;
  FSelectIBFrame.UseExisting := Info.UseExisting;

  FSelectSourceFrame.ConnectionString := Info.SourceConnectionString;
end;

procedure TWizardFlow.Next;
begin
  if FWizardStep = wsExecute then
    Application.Terminate
  else begin
    ForwardFrom(WizardStep);
    WizardStep := Succ(WizardStep);
  end;
end;

procedure TWizardFlow.SaveDefaultSource(Strings: TStrings);
begin
  Strings.SaveToFile(GetDefaultSourceFileName);
end;

procedure TWizardFlow.SaveToScript(const FileName: string);
begin
  FConvertInfo.SaveToScript(FileName);
end;

procedure TWizardFlow.ScriptingFeedback(Sender: TObject; Text: string);
begin
  FCustomize.Feedback(Text);
end;

procedure TWizardFlow.SetWizardStep(const Value: TWizardStep);
begin
  case Value of
    Low(Value): begin
      FBackButton.Enabled := False;
    end;
    Succ(Low(Value)): begin
      FBackButton.Enabled := True;
    end;
    Pred(High(Value)): begin
      FNextButton.Caption := '&Next >';
      FNextButton.Enabled := True;
    end;
    High(Value): begin
      FNextButton.Caption := '&Finish';
      FNextButton.Enabled := False;
    end;
  end;
  FWizardFrames[FWizardStep].Visible := False;
  FWizardStep := Value;
  FWizardFrames[Value].Visible := True
end;

procedure TWizardFlow.ShowErrors(Sender: TObject; Strings: TStrings);
begin
  FMigrateDBFrame.ShowErrors(Strings);
end;

end.

