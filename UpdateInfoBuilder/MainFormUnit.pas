unit MainFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, StdCtrls, ExtCtrls, IBUpdateUnit, Menus;

type
  TMainForm = class(TForm)
    Actions: TActionList;
    LoadDocument: TAction;
    SaveDocument: TAction;
    AddTopic: TAction;
    DeleteTopic: TAction;
    pnlDocument: TPanel;
    pnlTopics: TPanel;
    pnlButtons: TPanel;
    pnlTopic: TPanel;
    lblMajorVersion: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    txtMajorVersion: TEdit;
    txtMinorVersion: TEdit;
    txtLastUpdate: TEdit;
    SetLastUpdate: TAction;
    NewDocument: TAction;
    lblTopics: TLabel;
    lstTopics: TListBox;
    lblName: TLabel;
    lblDescription: TLabel;
    lblLastUpdate: TLabel;
    lblBody: TLabel;
    SetTopicUpdate: TAction;
    txtName: TEdit;
    txtDescription: TEdit;
    txtTopicLastUpdate: TEdit;
    txtBody: TMemo;
    btnSetLastUpdate: TButton;
    btnSetTopicUpdate: TButton;
    TopicsPopup: TPopupMenu;
    AddTopic1: TMenuItem;
    DeleteTopic1: TMenuItem;
    btnNewDocument: TButton;
    btnLoadDocument: TButton;
    btnSaveDocument: TButton;
    procedure NewDocumentExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetLastUpdateExecute(Sender: TObject);
    procedure AddTopicExecute(Sender: TObject);
    procedure lstTopicsClick(Sender: TObject);
    procedure SetTopicUpdateUpdate(Sender: TObject);
    procedure SetTopicUpdateExecute(Sender: TObject);
    procedure SaveDocumentExecute(Sender: TObject);
    procedure LoadDocumentExecute(Sender: TObject);
  private
    FDocument: TIBUpdateDocument;
    FPreviousTopic: TIBUpdateTopic;
    FPreviousTopicIndex: Integer;
    procedure UpdateFromDocument;
    procedure UpdateFromTopic;
    procedure UpdateToDocument;
    procedure UpdateToTopic(Topic: TIBUpdateTopic);
    function GetActiveTopic: TIBUpdateTopic;
  public
    property ActiveTopic: TIBUpdateTopic read GetActiveTopic;
    property Document: TIBUpdateDocument read FDocument;
    property PreviousTopic: TIBUpdateTopic read FPreviousTopic write FPreviousTopic;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  DialogsUtils;

procedure TMainForm.NewDocumentExecute(Sender: TObject);
begin
  PreviousTopic := nil;
  FPreviousTopicIndex := -1;
  FDocument.Clear;
  lstTopics.Clear;

  SetLastUpdate.Execute;
  AddTopic.Execute;
  UpdateFromDocument;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDocument := TIBUpdateDocument.Create;
  NewDocument.Execute;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDocument.Free;
end;

procedure TMainForm.UpdateFromTopic;
var
  Topic: TIBUpdateTopic;
begin
  Topic := ActiveTopic;
  if Topic = nil then begin
    txtName.Clear;
    txtDescription.Clear;
    txtTopicLastUpdate.Clear;
    txtBody.Clear;
  end else begin
    txtName.Text := Topic.Name;
    txtDescription.Text := Topic.Description;
    txtTopicLastUpdate.Text := MakeStdDate(Topic.LastUpdate);
    txtBody.Lines := Topic.Body;
  end;
end;

procedure TMainForm.UpdateFromDocument;
begin
  txtMajorVersion.Text := IntToStr(FDocument.MajorVersion);
  txtMinorVersion.Text := IntToStr(FDocument.MinorVersion);
  txtLastUpdate.Text := MakeStdDate(FDocument.LastUpdate);
end;

procedure TMainForm.SetLastUpdateExecute(Sender: TObject);
begin
  FDocument.LastUpdate := Now;
  txtLastUpdate.Text := MakeStdDate(FDocument.LastUpdate);
end;

procedure TMainForm.AddTopicExecute(Sender: TObject);
var
  Topic: TIBUpdateTopic;
begin
  Topic := TIBUpdateTopic.Create;
  Topic.Name := 'NEWTOPIC';
  Topic.Description := 'ThisThat Tool';
  Topic.LastUpdate := Now;
  Topic.Body.Add('Hello, world!');
  FDocument.Add(Topic);
  lstTopics.ItemIndex := lstTopics.Items.AddObject(Topic.Name, Topic);
  UpdateFromTopic;
end;

procedure TMainForm.lstTopicsClick(Sender: TObject);
begin
  try
    UpdateToTopic(PreviousTopic);
  except
    lstTopics.ItemIndex := FPreviousTopicIndex;
    raise;
  end;
  UpdateFromTopic;
  PreviousTopic := ActiveTopic;
  FPreviousTopicIndex := lstTopics.ItemIndex;
end;

procedure TMainForm.SetTopicUpdateUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ActiveTopic <> nil;
end;

function TMainForm.GetActiveTopic: TIBUpdateTopic;
begin
  if lstTopics.ItemIndex > -1 then
    Result := lstTopics.Items.Objects[lstTopics.ItemIndex] as TIBUpdateTopic
  else
    Result := nil;
end;

procedure TMainForm.SetTopicUpdateExecute(Sender: TObject);
begin
  ActiveTopic.LastUpdate := Now;
  txtTopicLastUpdate.Text := MakeStdDate(ActiveTopic.LastUpdate);
end;

procedure TMainForm.UpdateToTopic(Topic: TIBUpdateTopic);
begin
  if Topic = nil then Exit;
  Topic.Name := txtName.Text;
  Topic.Description := txtDescription.Text;
  Topic.LastUpdate := GetStdDate(txtTopicLastUpdate.Text);
  Topic.Body := txtBody.Lines;
end;

procedure TMainForm.SaveDocumentExecute(Sender: TObject);
var
  FileName: string;
  Strings: TStringList;
begin
  UpdateToDocument;
  UpdateToTopic(ActiveTopic);
  
  FileName := GetSaveTextFile;
  Strings := TStringList.Create;
  try
    FDocument.SaveToStrings(Strings);
    Strings.SaveToFile(FileName);
  finally
    Strings.Free;
  end;
end;

procedure TMainForm.LoadDocumentExecute(Sender: TObject);
var
  FileName: string;
  i: Integer;
  Topic: TIBUpdateTopic;
  Strings: TStringList;
begin
  PreviousTopic := nil;
  FPreviousTopicIndex := -1;
  FDocument.Clear;
  lstTopics.Clear;

  FileName := GetOpenTextFile;
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(FileName);
    FDocument.LoadFromStrings(Strings);
  finally
    Strings.Free;
  end;

  for i := 0 to Document.TopicCount - 1 do begin
    Topic := Document.Topics[i];
    lstTopics.Items.AddObject(Topic.Name, Topic);
  end;
  UpdateFromDocument;
  UpdateFromTopic;
end;

procedure TMainForm.UpdateToDocument;
begin
  FDocument.MajorVersion := StrToInt(txtMajorVersion.Text);
  FDocument.MinorVersion := StrToInt(txtMinorVersion.Text);
  FDocument.LastUpdate := GetStdDate(txtLastUpdate.Text);
end;

end.
