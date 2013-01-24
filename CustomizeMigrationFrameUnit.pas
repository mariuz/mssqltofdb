{*******************************************************}
{                                                       }
{    Customize Migration Frame                          }
{                                                       }
{    Provides controls to customize the ADO to          }
{    InterBase migration process.                       }
{                                                       }
{*******************************************************}

unit CustomizeMigrationFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ConvertInfoUnit, StdCtrls, CheckLst;

type
  TCheckboxOption = (coUseDialect3, coMigrateAutoNumber, coSkipMetadata,
    coSkipTrigger, coCreateGUID, coInterruptOnErrors);
  { defaults used to be: use dialect 3, migrate autonumeric, coCreateGUID }

  TCustomizeMigrationFrame = class(TFrame)
    lblTitle: TLabel;
    lblHeader: TLabel;
    grpCustomizations: TGroupBox;
    lblScriptingFeedback: TLabel;
    btnSelectTables: TButton;
    lblNameMapMethod: TLabel;
    cmbMapMethod: TComboBox;
    lblCharSet: TLabel;
    cmbCharSet: TComboBox;
    btnSelectLibraries: TButton;
    btnSelectDefaults: TButton;
    lstOptions: TCheckListBox;
    procedure btnSelectTablesClick(Sender: TObject);
    procedure btnSelectLibrariesClick(Sender: TObject);
    procedure btnSelectDefaultsClick(Sender: TObject);
    procedure lstOptionsClickCheck(Sender: TObject);
  private
    FDefaultValues: TStrings;
    FLibraries: TStrings;
    FTables: TStrings;
    function GetCharSet: string;
    function GetCreateGUIDDomain: Boolean;
    function GetDefaultValues: TStrings;
    function GetInterruptOnErrors: Boolean;
    function GetMapMethod: TMapMethod;
    function GetMigrateAutoNumber: Boolean;
    function GetLibraries: TStrings;
    function GetOption(Option: TCheckboxOption): Boolean;
    function GetSkipMetadata: Boolean;
    function GetTables: TStrings;
    function GetUseDialect3: Boolean;
    procedure SetCharSet(const Value: string);
    procedure SetCreateGUIDDomain(const Value: Boolean);
    procedure SetDefaultValues(Value: TStrings);
    procedure SetInterruptOnErrors(const Value: Boolean);
    procedure SetLibraries(const Value: TStrings);
    procedure SetMapMethod(const Value: TMapMethod);
    procedure SetMigrateAutoNumber(const Value: Boolean);
    procedure SetOption(Option: TCheckboxOption; const Value: Boolean);
    procedure SetSkipMetadata(const Value: Boolean);
    procedure SetTables(const Value: TStrings);
    procedure SetUseDialect3(const Value: Boolean);
    function GetSkipTriggers: Boolean;
    procedure SetSkipTriggers(const Value: Boolean);
  public
    destructor Destroy; override;
    procedure Feedback(const Text: string);
    procedure StartFeedback;
    procedure StopFeedback;

    property CharSet: string read GetCharSet write SetCharSet;
    property CreateGUIDDomain: Boolean read GetCreateGUIDDomain write SetCreateGUIDDomain;
    property DefaultValues: TStrings read GetDefaultValues write SetDefaultValues;
    property InterruptOnErrors: Boolean read GetInterruptOnErrors write SetInterruptOnErrors;
    property Libraries: TStrings read GetLibraries write SetLibraries;
    property MapMethod: TMapMethod read GetMapMethod write SetMapMethod;
    property MigrateAutoNumber: Boolean read GetMigrateAutoNumber write SetMigrateAutoNumber;
    property SkipMetadata: Boolean read GetSkipMetadata write SetSkipMetadata;
    property SkipTriggers: Boolean read GetSkipTriggers write SetSkipTriggers;
    property Tables: TStrings read GetTables write SetTables;
    property UseDialect3: Boolean read GetUseDialect3 write SetUseDialect3;
  end;

implementation

uses
  AdoUtils, CheckLstUtils, FormsUtils,
  WizFlowUnit, MLRFileIteratorUnit, SQL2GDBConstsUnit,
  DefaultValuesFormUnit;

{$R *.DFM}

const
  MapMethodIndexes: array[mmUnderscores..mmUseQuoted] of Integer = (
    1, { Use underscores }
    2, { Remove whitespace }
    0  { Use quotes (dialect 3 only) }
  );

  SUnknownMapMethodIndex = 'Unknown map method index: %d';

function IndexToMapMethod(const Index: Integer): TMapMethod;
var
  i: TMapMethod;
begin
  for i := Low(MapMethodIndexes) to High(MapMethodIndexes) do
    if MapMethodIndexes[i] = Index then begin
      Result := i;
      Exit;
    end;
  raise Exception.CreateFmt(SUnknownMapMethodIndex, [Index]);
end;

function MapMethodToIndex(const MapMethod: TMapMethod): Integer;
begin
  Result := MapMethodIndexes[MapMethod];
end;

{ TCustomizeMigrationFrame }

destructor TCustomizeMigrationFrame.Destroy;
begin
  FreeAndNil(FDefaultValues);
  FreeAndNil(FLibraries);
  FreeAndNil(FTables);
  inherited;
end;

procedure TCustomizeMigrationFrame.Feedback(const Text: string);
begin
  lblScriptingFeedback.Caption := Text;
  Refresh;
end;

function TCustomizeMigrationFrame.GetCreateGUIDDomain: Boolean;
begin
  Result := GetOption(coCreateGUID);
end;

function TCustomizeMigrationFrame.GetDefaultValues: TStrings;
begin
  if not Assigned(FDefaultValues) then
    FDefaultValues := TStringList.Create;
  Result := FDefaultValues;
end;

function TCustomizeMigrationFrame.GetMigrateAutoNumber: Boolean;
begin
  Result := GetOption(coMigrateAutoNumber);
end;

function TCustomizeMigrationFrame.GetSkipMetadata: Boolean;
begin
  Result := GetOption(coSkipMetadata);
end;

function TCustomizeMigrationFrame.GetTables: TStrings;
begin
  if not Assigned(FTables) then
    FTables := TStringList.Create;
  Result := FTables;
end;

function TCustomizeMigrationFrame.GetUseDialect3: Boolean;
begin
  Result := GetOption(coUseDialect3);
end;

procedure TCustomizeMigrationFrame.SetCreateGUIDDomain(
  const Value: Boolean);
begin
  SetOption(coCreateGUID, Value);
end;

procedure TCustomizeMigrationFrame.SetDefaultValues(Value: TStrings);
begin
  GetDefaultValues.Assign(Value);
end;

procedure TCustomizeMigrationFrame.SetMigrateAutoNumber(
  const Value: Boolean);
begin
  SetOption(coMigrateAutoNumber, Value);
end;

procedure TCustomizeMigrationFrame.SetSkipMetadata(const Value: Boolean);
begin
  SetOption(coSkipMetadata, Value);
end;

procedure TCustomizeMigrationFrame.SetTables(const Value: TStrings);
begin
  if not Assigned(FTables) then
    FTables := TStringList.Create;
  FTables.Assign(Value);
end;

procedure TCustomizeMigrationFrame.SetUseDialect3(const Value: Boolean);
begin
  SetOption(coUseDialect3, Value);
end;

procedure TCustomizeMigrationFrame.btnSelectTablesClick(Sender: TObject);
begin
  ConnectionSelectTables(
    ADOOpenConnect(Flow.ConvertInfo.SourceConnectionString), Tables);
end;

function TCustomizeMigrationFrame.GetMapMethod: TMapMethod;
begin
  Result := IndexToMapMethod(cmbMapMethod.ItemIndex);
end;

procedure TCustomizeMigrationFrame.SetMapMethod(const Value: TMapMethod);
begin
  cmbMapMethod.ItemIndex := MapMethodToIndex(Value);
end;

function TCustomizeMigrationFrame.GetCharSet: string;
begin
  Result := cmbCharSet.Text;
end;

procedure TCustomizeMigrationFrame.SetCharSet(const Value: string);
begin
  cmbCharSet.Text := Value;
end;

function TCustomizeMigrationFrame.GetInterruptOnErrors: Boolean;
begin
  Result := GetOption(coInterruptOnErrors);
end;

procedure TCustomizeMigrationFrame.SetInterruptOnErrors(
  const Value: Boolean);
begin
  SetOption(coInterruptOnErrors, Value);
end;

function SelectStrings(Strings: TStrings;
  Selection: TStrings; const Prompt: string = ''): Boolean;
var
  CheckListBox: TCheckListBox;
  Form: TForm;
begin
  Form := CreateControlDialog(CheckListBox, TCheckListBox);
  try
    Form.Caption := Prompt;
    CheckListBox.Items := Strings;
    CheckItems(CheckListBox, Selection);
    Result := Form.ShowModal = mrOk;
    if Result then begin
      Selection.Clear;
      ListCheckedItems(CheckListBox, Selection);
    end;
  finally
    Form.Free;
  end;
end;

procedure TCustomizeMigrationFrame.btnSelectLibrariesClick(
  Sender: TObject);
var
  AFileName: string;
  i: Integer;
  Strings: TStringList;
begin
  { See sql2gdb_udfs.htm for more information. }
  Strings := TStringList.Create;
  try
    ListFiles(Strings, ExtractFilePath(Application.ExeName),
      '*' + LibraryEndMask, False);
    if Strings.Count = 0 then
      raise Exception.Create(SNoLibrariesToSelect);
    for i := 0 to Strings.Count - 1 do begin
      AFileName := Strings[i];
      Strings[i] := Copy(AFileName, 1,
        Length(AFileName) - Length(LibraryEndMask));
    end;
    SelectStrings(Strings, Libraries, 'Select libraries to declare');
  finally
    Strings.Free;
  end;
end;

function TCustomizeMigrationFrame.GetLibraries: TStrings;
begin
  if not Assigned(FLibraries) then
    FLibraries := TStringList.Create;
  Result := FLibraries;
end;

procedure TCustomizeMigrationFrame.SetLibraries(const Value: TStrings);
begin
  GetLibraries.Assign(Value);
end;

procedure TCustomizeMigrationFrame.btnSelectDefaultsClick(Sender: TObject);
begin
  EditDefaultValues(DefaultValues);
end;

function TCustomizeMigrationFrame.GetSkipTriggers: Boolean;
begin
  Result := GetOption(coSkipTrigger);
end;

procedure TCustomizeMigrationFrame.SetSkipTriggers(const Value: Boolean);
begin
  SetOption(coSkipTrigger, Value);
end;

function TCustomizeMigrationFrame.GetOption(
  Option: TCheckboxOption): Boolean;
begin
  Result := lstOptions.Checked[Ord(Option)];
end;

procedure TCustomizeMigrationFrame.SetOption(Option: TCheckboxOption;
  const Value: Boolean);
begin
  lstOptions.Checked[Ord(Option)] := Value;
end;

procedure TCustomizeMigrationFrame.StartFeedback;
begin
  lblScriptingFeedback.Visible := True;
  grpCustomizations.Visible := False;
end;

procedure TCustomizeMigrationFrame.StopFeedback;
begin
  lblScriptingFeedback.Visible := False;
  grpCustomizations.Visible := True;
end;

procedure TCustomizeMigrationFrame.lstOptionsClickCheck(Sender: TObject);
var
  Option: TCheckboxOption;
  Value: Boolean;
begin
  if lstOptions.ItemIndex = -1 then Exit;
  Option := TCheckboxOption(lstOptions.ItemIndex);
  Value := GetOption(Option);
  case Option of
    coUseDialect3: begin
      if (not Value) and (MapMethod = mmUseQuoted) then begin
        Feedback('Not using dialect 3 implies not using quoted identifiers.');
        MapMethod := mmUnderscores;
      end;
    end;
    coMigrateAutoNumber: begin
      if (not Value) and (not GetOption(coSkipTrigger)) then begin
        Feedback('Removing AutoNumber support implies not generating triggers.');
        SetOption(coSkipTrigger, True);
      end;
    end;
    coSkipMetadata: begin
      if Value and not GetOption(coSkipTrigger) then begin
        Feedback('Skipping metadata implies not generating triggers.');
        SetOption(coSkipTrigger, True);
      end;
    end;
    coSkipTrigger: begin
      if (not Value) and GetOption(coSkipMetadata) then begin
        Feedback('Generating triggers implies generating metadata.');
        SetOption(coSkipMetadata, False);
      end;
    end;
  end;
end;

end.
