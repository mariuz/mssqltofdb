{*****************************************************************************}
{                                                                             }
{ Text Schema Wizard Unit                                                     }
{ Provides a Wizard to generate a text schema file.                           }
{                                                                             }
{*****************************************************************************}

unit TextFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, FileCtrl, Math, TextDelimiterFrameUnit,
  TextPreviewFrameUnit, TextFieldsFrameUnit, TextModelUnit;

type
  TTextWizardFlow = class
  private
    FBackButton: TButton;
    FCancelButton: TButton;
    FNextButton: TButton;
    FTextInfo: TTextInfo;
    FTextDelimiterFrame: TTextDelimiterFrame;
    FTextPreviewFrame: TTextPreviewFrame;
    FTextFieldsFrame: TTextFieldsFrame;
    FTextWizardFrames: array[twsTextPreview .. twsTextFields] of TFrame;
    FTextWizardStep: TTextWizardStep;
    FOnFinish: TNotifyEvent;
    procedure TextFieldsSave(Sender: TObject);
    procedure ForwardFrom(const Step: TTextWizardStep);
    procedure SetTextWizardStep(const Value: TTextWizardStep);
  public
    constructor Create(Parent: TWinControl;
      BackButton, NextButton, CancelButton: TButton);
    destructor Destroy; override;
    procedure Back;
    procedure Next;

    property TextInfo: TTextInfo read FTextInfo;
    property TextWizardStep: TTextWizardStep read FTextWizardStep write SetTextWizardStep;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

  TTextForm = class(TForm)
    TextPanel: TPanel;
    btnCancel: TButton;
    btnBack: TButton;
    btnNext: TButton;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFlow: TTextWizardFlow;
    function GetTextInfo: TTextInfo;
    procedure FlowFinish(Sender: TObject);
  public
    property Flow: TTextWizardFlow read FFlow;
    property TextInfo: TTextInfo read GetTextInfo;
  end;

{ Executes the Text Schema wizard. Folder is used to suggest a directory. }
procedure ExecuteTextWizard(var Folder: string);

implementation

{$R *.DFM}

procedure ExecuteTextWizard(var Folder: string);
var
  Form: TTextForm;
begin
  Form := TTextForm.Create(Application);
  try
    { TODO:use the Folder parameter. }
    if Form.ShowModal = mrOk then begin
    end;
  finally
    Form.Free;
  end;
end;

{ TTextForm }

procedure TTextForm.FormCreate(Sender: TObject);
const
  BottomRightAnchors = [akBottom, akRight];
begin
  Height := Application.MainForm.Height;
  Width := Application.MainForm.Width;
  
  btnCancel.Top := ClientHeight - 8 - btnCancel.Height;
  btnCancel.Left := ClientWidth - 8 - btnCancel.Width;
  btnCancel.Anchors := BottomRightAnchors;

  btnNext.Top := btnCancel.Top;
  btnNext.Left := btnCancel.Left - 8 - btnNext.Width;
  btnNext.Anchors := BottomRightAnchors;

  btnBack.Top := btnCancel.Top;
  btnBack.Left := btnNext.Left - btnBack.Width;
  btnBack.Anchors := BottomRightAnchors;

  Bevel1.Top := btnCancel.Top - 8 - Bevel1.Height;
  Bevel1.Left := 8;
  Bevel1.Width := ClientWidth - 16;
  Bevel1.Anchors := [akLeft, akBottom, akRight];

  TextPanel.Top := 8;
  TextPanel.Left := 8;
  TextPanel.Height := Bevel1.Top - 8 - btnCancel.Height;
  TextPanel.Width := ClientWidth - 16;
  TextPanel.Anchors := [akLeft, akTop, akBottom, akRight];

  FFlow := TTextWizardFlow.Create(TextPanel, btnBack, btnNext, btnCancel);
  FFlow.OnFinish := FlowFinish;
end;

procedure TTextForm.btnNextClick(Sender: TObject);
begin
  Flow.Next;
end;

procedure TTextForm.btnBackClick(Sender: TObject);
begin
  Flow.Back;
end;

procedure TTextForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

{ TTextWizard }

constructor TTextWizardFlow.Create(Parent: TWinControl; BackButton,
  NextButton, CancelButton: TButton);
var
  FrameInfo: ITextInfoSupport;
  TextStep: TTextWizardStep;
begin
  FTextInfo := TTextInfo.Create;

  FBackButton := BackButton;
  FNextButton := NextButton;
  FCancelButton := CancelButton;

  FTextPreviewFrame := TTextPreviewFrame.Create(Parent);
  FTextWizardFrames[twsTextPreview] := FTextPreviewFrame;

  FTextDelimiterFrame := TTextDelimiterFrame.Create(Parent);
  FTextWizardFrames[twsTextDelimiter] := FTextDelimiterFrame;

  FTextFieldsFrame := TTextFieldsFrame.Create(Parent);
  FTextFieldsFrame.OnSave := TextFieldsSave;
  FTextWizardFrames[twsTextFields] := FTextFieldsFrame;

  { Assign parents. }
  for TextStep := Low(FTextWizardFrames) to High(FTextWizardFrames) do begin
    FTextWizardFrames[TextStep].Visible := False;
    FTextWizardFrames[TextStep].Parent := Parent;
    FTextWizardFrames[TextStep].Align := alClient;
    if FTextWizardFrames[TextStep].GetInterface(ITextInfoSupport, FrameInfo) then
      FrameInfo.TextInfo := TextInfo;
  end;
  TextWizardStep := twsTextPreview;
  FTextPreviewFrame.EnableDragDrop;
end;

destructor TTextWizardFlow.Destroy;
begin
  { Allow the Parent object to destroy all the frames. }
  inherited;
end;

procedure TTextWizardFlow.Next;
begin
  if FTextWizardStep = High(TTextWizardStep) then begin
    if Assigned(FOnFinish) then
      FOnFinish(Self);
  end else begin
    ForwardFrom(TextWizardStep);
    TextWizardStep := Succ(TextWizardStep);
  end;
end;

procedure TTextWizardFlow.Back;
begin
  TextWizardStep := Pred(TextWizardStep);
end;

procedure TTextWizardFlow.SetTextWizardStep(const Value: TTextWizardStep);
begin
  case Value of
    Low(Value): begin
      FBackButton.Enabled := False;
    end;
    Succ(Low(Value)): begin
      FBackButton.Enabled := True;
      FNextButton.Caption := '&Next >';
      FNextButton.Enabled := True;
      FCancelButton.Enabled := True;
    end;
    { In this case, Succ(Low(Value)) = Pred(High(Value)) }
    {
    Pred(High(Value)): begin
      FNextButton.Caption := '&Next >';
      FNextButton.Enabled := True;
    end;
    }
    High(Value): begin
      FNextButton.Caption := '&Finish';
      FNextButton.Enabled := False;
      FCancelButton.Enabled := True;
    end;
  end;
  FTextWizardFrames[FTextWizardStep].Visible := False;
  FTextWizardStep := Value;
  FTextWizardFrames[Value].Visible := True
end;

procedure TTextWizardFlow.ForwardFrom(const Step: TTextWizardStep);
begin
  case Step of
    twsTextPreview: begin
      if TextInfo.TextFileName = '' then
        raise Exception.Create('You must select a text file to continue');
      if FileExists(TextInfo.SchemaFileName) then
        TextInfo.LoadSchemaFromFile
      else
        TextInfo.GuessFromFileName;
      FTextDelimiterFrame.ReadFromTextInfo;
    end;
    twsTextDelimiter: begin
      FTextDelimiterFrame.WriteToTextInfo;
      if TextInfo.Guess then
        TextInfo.GuessFromFileData;
      FTextFieldsFrame.LoadValuesToGrid;
    end;
    twsTextFields:
      ;
  end;
end;

function TTextForm.GetTextInfo: TTextInfo;
begin
  Result := Flow.TextInfo;
end;

procedure TTextForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFlow);
end;

procedure TTextForm.FlowFinish(Sender: TObject);
begin
  Close;
end;

procedure TTextWizardFlow.TextFieldsSave(Sender: TObject);
begin
  FCancelButton.Enabled := False;
  FNextButton.Enabled := True;
end;

end.
