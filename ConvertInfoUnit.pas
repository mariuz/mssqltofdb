
{*******************************************************}
{                                                       }
{     Convertion Information Library                    }
{                                                       }
{     Provides classes and routines to convert          }
{     from an ADO database to an InterBase database.    }
{                                                       }
{*******************************************************}

unit ConvertInfoUnit;

interface

uses
  ADODB_TLB, ADOX_TLB, Classes, ComObj, Db, SysUtils;

type

{ Event type declarations. }
  TFeedbackEvent = procedure (Sender: TObject; Text: string) of object;
  TShowErrorsEvent = procedure (Sender: TObject; Errors: TStrings) of object;

{ Forward declarations. }
{$TYPEINFO ON}
  TConvertInfo = class;
{$TYPEINFO OFF}

{ Well-known providers. }
  TOleDbProvider = (opJet, opSQLServer, opUnknown);

{ Output types for the conversion. }
  TOutputType = (otIB, otText);

{ TNameMap manages mapping table and column names from the source
  database to the target database. }

  TSimpleMap = TStringList;
  TMapMethod = (mmUnderscores, mmRemoveWhiteSpace, mmUseQuoted);
  TMapMethods = set of TMapMethod;

  TNameMap = class
  private
    FSrcToDestCol: TSimpleMap;
    FDestToSrcCol: TSimpleMap;
    FConvertInfo: TConvertInfo;
    FSrcToDestTable: TSimpleMap;
    FDestToSrcTable: TSimpleMap;
    FReservedWords: TStringList;
    FMapMethods: TMapMethods;
    function MangleIdentifier(const Identifier, Suffix: string): string;
    function MangleColumnName(ColumnName: string): string;
    function MangleTableName(const TableName: string): string;
    procedure LoadReservedWords;
  public
    constructor Create(ConvertInfo: TConvertInfo);
    destructor Destroy; override;

    procedure Clear;
    function GetColName(const ColumnName: string): string;
    function GetTableName(const TableName: string): string;
    function IsReservedWord(Token: string): Boolean;
    function IsSimpleIdentifier(const Token: string): Boolean;
    procedure LoadFromString(const S: string);
    function SaveToString: string;
    function UnmangeColumnName(const ColumnName: string): string;
    function UnmangleTableName(const TableName: string): string;
    property MapMethods: TMapMethods read FMapMethods write FMapMethods;
  end;

{ Information required to make the conversion from an ADO source
  to Interbase. The scripts are stored as a TStringList inside the
  Scripts property, which is indexed by a generated,
  user-friendly name. }
{$TYPEINFO ON}
  TConvertInfo = class
  private
    FCatalog: _Catalog;
    FCharSet: string;
    FCreateGUIDDomain: Boolean;
    FDeleteExisting: Boolean;
    FDefaultValues: TStrings;
    FIBFileName: string;
    FIBPassword: string;
    FIBUserName: string;
    FInterruptOnErrors: Boolean;
    FLibraries: TStrings;
    FMapMethods: TMapMethods;
    FMigrateAutoNumber: Boolean;
    FNameMap: TNameMap;
    FOleDbProvider: TOleDbProvider;
    FOutputFileName: string;
    FOutputType: TOutputType;
    FRestrictToTables: TStrings;
    FScripts: TStrings;
    FSkipMetadata: Boolean;
    FSkipTriggers: Boolean;
    FSourceConnectionString: string;
    FUseDialect3: Boolean;
    FUseExisting: Boolean;
    FOnExecutionFeedback: TFeedbackEvent;
    FOnFinish: TNotifyEvent;
    FOnScriptingFeedback: TFeedbackEvent;
    FOnShowErrors: TShowErrorsEvent;
    procedure AddSimpleScript(const ScriptTitle, ScriptBody: string);
    procedure CheckProvider;
    function ColumnsToString(AColumns: Columns): string;
    procedure ConnectToSourceCatalog;
    procedure DisconnectCatalog;
    procedure GenerateCheckScripts;
    procedure GenerateDomainScripts;
    procedure GenerateGeneratorScripts;
    procedure GenerateIndexScripts;
    procedure GenerateKeyScripts;
    procedure GeneratePumpScripts;
    procedure GenerateQueryScripts;
    procedure GenerateTableScripts;
    procedure GenerateTriggerScripts;
    procedure GenerateUDFScripts;
    function IsAutoInc(ATable: Table; AColumn: Column): Boolean;
    procedure ListColumns(AColumns: Columns; Strings: TStrings);
    procedure ListRelatedColumns(AColumns: Columns; Strings: TStrings);
    procedure ListPrefixRelatedColumns(const Prefix: string;
      AColumns: Columns; Strings: TStrings);
    procedure ListPrefixColumns(const Prefix: string;
      AColumns: Columns; Strings: TStrings);
    function MaxFieldValue(const TableName, FieldName: string): Integer;
    function MustMigrateTable(const TableName: string): Boolean;
    function OpenRecordset(const CommandText: string): _Recordset;
    procedure ScriptFeedback(const Text: string);
    procedure SetDefaultValues(Value: TStrings);
    procedure SetRestrictToTables(const Value: TStrings);
    procedure SetLibraries(const Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearScripts;
    procedure Execute;
    procedure GenerateScripts;
    function GetColName(const ColumnName: string;
      const ForComposition: Boolean = True): string;
    function GetIndexName(const IndexName: string): string;
    function GetProviderFieldName(const FieldName: string): string;
    function GetProviderTableName(const TableName: string): string;
    function GetTableName(const TableName: string;
      const ForComposition: Boolean = True): string;
    function IsReservedWord(Token: string): Boolean;
    procedure LoadFromScript(const FileName: string);
    procedure SaveToScript(const FileName: string);
    function UnmangeColumnName(const ColumnName: string): string;
    function UnmangleTableName(const TableName: string): string;

  published
    property CharSet: string read FCharSet write FCharSet;
    property CreateGUIDDomain: Boolean read FCreateGUIDDomain write FCreateGUIDDomain;
    property DefaultValues: TStrings read FDefaultValues write SetDefaultValues;
    property DeleteExisting: Boolean read FDeleteExisting write FDeleteExisting;
    property IBFileName: string read FIBFileName write FIBFileName;
    property IBPassword: string read FIBPassword write FIBPassword;
    property IBUserName: string read FIBUserName write FIBUserName;
    property InterruptOnErrors: Boolean read FInterruptOnErrors write FInterruptOnErrors;
    property Libraries: TStrings read FLibraries write SetLibraries;
    property MapMethods: TMapMethods read FMapMethods write FMapMethods;
    property MigrateAutoNumber: Boolean read FMigrateAutoNumber write FMigrateAutoNumber;
    property NameMap: TNameMap read FNameMap;
    property OutputFileName: string read FOutputFileName write FOutputFileName;
    property OutputType: TOutputType read FOutputType write FOutputType;
    property RestrictToTables: TStrings read FRestrictToTables write SetRestrictToTables;
    property Scripts: TStrings read FScripts;
    property SkipMetadata: Boolean read FSkipMetadata write FSkipMetadata;
    property SkipTriggers: Boolean read FSkipTriggers write FSkipTriggers;
    property SourceConnectionString: string read FSourceConnectionString write FSourceConnectionString;
    property UseDialect3: Boolean read FUseDialect3 write FUseDialect3;
    property UseExisting: Boolean read FUseExisting write FUseExisting;

    property OnExecutionFeedback: TFeedbackEvent read FOnExecutionFeedback write FOnExecutionFeedback;
    property OnScriptingFeedback: TFeedbackEvent read FOnScriptingFeedback write FOnScriptingFeedback;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnShowErrors: TShowErrorsEvent read FOnShowErrors write FOnShowErrors;
  end;
{$TYPEINFO OFF}  

{ Exported functions. }

function Delimited(const AString: string): string;
function GetIdentifierPrefix(const IncludeNr: Boolean): String;
function FieldIsBlob(Field: Field): Boolean;
function FieldTypeIsBlob(FieldType: DataTypeEnum): Boolean;
function FieldTypeIsNumeric(FieldType: DataTypeEnum): Boolean;
function FieldTypeIsString(FieldType: DataTypeEnum): Boolean;
function FirstMapMethod(Methods: TMapMethods): TMapMethod;
function OleDbTypeToFieldType(ColType: DataTypeEnum): TFieldType;

implementation

uses
  Controls, Dialogs, Forms, MigrationUnit, StdCtrls, SQL2GDBConstsUnit,
  TypInfo;

const
  { Errors we might want to trap. }
  IncapableProvider = -2146825037;
  PropertyNotFound = -2146825023;

const
  { Important character sets. }
  AlphaChars = ['a'..'z', 'A'..'Z'];
  FriendlyChars = ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
  SimpleChars = ['a'..'z', 'A'..'Z', '0'..'9', '_'];

var
  IdentifierCount: Integer;    

function CreateStrings(const Text: string): TStrings;
begin
  Result := TStringList.Create;
  Result.Text := Text;
end;

function Delimited(const AString: string): string;
begin
  Result := '"' + StringReplace(AString, '"', '""', [rfReplaceAll]) + '"';
end;

function GetIdentifierPrefix(const IncludeNr: Boolean): String;
begin
  Result := SIdentifierPrefix + '_';
  if IncludeNr then begin
    Inc(IdentifierCount);
    Result := Result + IntToStr(IdentifierCount);
  end;
end;

procedure DumpProperties(Props: Properties);
var
  i: Integer;
  Strings: TStringList;
  Form: TForm;
  Memo: TMemo;
begin
  Form := nil;
  Strings := TStringList.Create;
  try
    for i := 0 to Props.Count - 1 do
      Strings.Add(Props.Item[i].Name + ': ' +
        VarToStr(Props.Item[i].Value));
    Form := TForm.Create(nil);
    Memo := TMemo.Create(Form);
    Memo.Parent := Form;
    Memo.Font.Name := 'Courier';
    Memo.ScrollBars := ssVertical;
    Memo.Align := alClient;
    Memo.Lines := Strings;
    Form.Position := poScreenCenter;
    Form.ShowModal;
  finally
    Strings.Free;
    Form.Free;
  end;
end;

function TruncateWithLeftTrim(const AString: string;
  const MaxLen: Integer): string;
begin
  if Length(AString) > MaxLen then
    Result := Copy(AString, Length(AString) - MaxLen, MaxLen) else
    Result := AString;
end;

function OleDbTypeToFieldType(ColType: DataTypeEnum): TFieldType;
begin
  case ColType of
    adTinyInt, adSmallInt:
      Result := ftSmallint;
    adInteger, adUnsignedTinyInt:
      Result := ftInteger;
    adUnsignedSmallInt:
      Result := ftWord;
    adSingle, adDouble, adDecimal, adNumeric:
      Result := ftFloat;
    adCurrency:
      Result := ftCurrency;
    adBoolean:
      Result := ftBoolean;
    adBigInt, adUnsignedBigInt:
      Result := ftLargeint;
    adGUID:
      Result := ftGUID;
    adDate, adDBDate:
      Result := ftDate;
    adDBTime:
      Result := ftTime;
    adDBTimeStamp:
      Result := ftDateTime;
    adBSTr, adWChar, adVarWChar:
      Result := ftWideString;
    adChar:
      Result := ftFixedChar;
    adVarChar:
      Result := ftString;
    adLongVarChar, adLongVarWChar:
      Result := ftMemo;
    adLongVarBinary:
      Result := ftBlob;
    adBinary:
      Result := ftBytes;
    adVarBinary:
      Result := ftVarBytes;
    adIDispatch:
      Result := ftIDispatch;
    adIUnknown:
      Result := ftInterface;
    adVariant:
      Result := ftVariant;
    else
      Result := ftUnknown;
  end;
end;

function OleDbTypeToString(ColType: DataTypeEnum): string;
begin
  case ColType of
    adEmpty:
      Result := 'Empty';
    adTinyInt:
      Result := 'TinyInt';
    adSmallInt:
      Result := 'SmallInt';
    adInteger:
      Result := 'Integer';
    adBigInt:
      Result := 'BigInt';
    adUnsignedTinyInt:
      Result := 'UnsignedTinyInt';
    adUnsignedSmallInt:
      Result := 'UnsignedSmallInt';
    adUnsignedInt:
      Result := 'UnsignedInt';
    adUnsignedBigInt:
      Result := 'UnsignedBigInt';
    adSingle:
      Result := 'Single';
    adDouble:
      Result := 'Double';
    adCurrency:
      Result := 'Currency';
    adDecimal:
      Result := 'Decimal';
    adNumeric:
      Result := 'Numeric';
    adBoolean:
      Result := 'Boolean';
    adError:
      Result := 'Error';
    adUserDefined:
      Result := 'UserDefined';
    adVariant:
      Result := 'Variant';
    adIDispatch:
      Result := 'IDispatch';
    adIUnknown:
      Result := 'IUnknown';
    adGUID:
      Result := 'GUID';
    adDate:
      Result := 'Date';
    adDBDate:
      Result := 'DBDate';
    adDBTime:
      Result := 'DBTime';
    adDBTimeStamp:
      Result := 'DBTimeStamp';
    adBSTR:
      Result := 'BSTR';
    adChar:
      Result := 'Char';
    adVarChar:
      Result := 'VarChar';
    adLongVarChar:
      Result := 'LongVarChar';
    adWChar:
      Result := 'WChar';
    adVarWChar:
      Result := 'VarWChar';
    adLongVarWChar:
      Result := 'LongVarWChar';
    adBinary:
      Result := 'Binary';
    adVarBinary:
      Result := 'VarBinary';
    adLongVarBinary:
      Result := 'LongVarBinary';
    adChapter:
      Result := 'Chapter';
    adFileTime:
      Result := 'FileTime';
    adPropVariant:
      Result := 'PropVariant';
    adVarNumeric:
      Result := 'VarNumeric';
    else
      Result := 'Unknown';
  end;
  Result := Result + ' (' + IntToStr(Integer(ColType)) + ')';
end;

function FieldIsBlob(Field: Field): Boolean;
begin
  Result := FieldTypeIsBlob(Field.Type_);
end;

function FieldTypeIsBlob(FieldType: DataTypeEnum): Boolean;
const
  BlobFields = [adLongVarChar, adLongVarWChar, adLongVarBinary];
begin
  Result := FieldType in BlobFields;
end;

function FieldTypeIsNumeric(FieldType: DataTypeEnum): Boolean;
const
  NumericFields = [adTinyInt, adSmallInt, adInteger,
    adBigInt, adUnsignedTinyInt, adUnsignedSmallInt,
    adUnsignedInt, adUnsignedBigInt, adSingle, adDouble,
    adCurrency, adDecimal, adNumeric, adVarNumeric];
begin
  Result := FieldType in NumericFields;
end;

function FieldTypeIsString(FieldType: DataTypeEnum): Boolean;
const
  StringFields = [adVariant, adGUID, adBSTR, adChar, adVarChar,
    adLongVarChar, adWChar, adVarWChar, adLongVarWChar];
begin
  Result := FieldType in StringFields;
end;

function FieldIsNumeric(Field: Field): Boolean;
begin
  Result := FieldTypeIsNumeric(Field.Type_);
end;

function FirstMapMethod(Methods: TMapMethods): TMapMethod;
begin
  for Result := Low(TMapMethod) to High(TMapMethod) do
    if Result in Methods then
      Exit;
  raise Exception.Create(SNoValidMethod);
end;

{ Loading & saving helper functions. }

function BoolToStr(const Value: Boolean): string;
begin
  if Value then
    Result := 'True' else
    Result := 'False';
end;

function StrToBool(const Value: string): Boolean;
begin
  if (Length(Value) > 0) and (UpCase(Value[1]) = 'T') then
    Result := True else
    Result := False;
end;

function LineToStr(const ALine: string): string;
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.CommaText := ALine;
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

function StrToLine(const AString: string): string;
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Text := AString;
    Result := Strings.CommaText;
  finally
    Strings.Free;
  end;
end;

function RemoveCommentTags(const ALine: string): string;
begin
  Result := Copy(ALine, 3, Length(ALine) - 4);
end;

{ TConvertInfo }

procedure TConvertInfo.AddSimpleScript(const ScriptTitle,
  ScriptBody: string);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Text := ScriptBody;
    FScripts.AddObject(ScriptTitle, Strings);
  except
    Strings.Free;
    raise;
  end;
end;

procedure TConvertInfo.CheckProvider;
begin
  if Pos(SJetProviderName, FSourceConnectionString) > 0 then
    FOleDbProvider := opJet
  else if Pos(SSQLServerProviderName, FSourceConnectionString) > 0 then
    FOleDbProvider := opSQLServer
  else
    FOleDbProvider := opUnknown;
end;

procedure TConvertInfo.ClearScripts;
var
  i: Integer;
begin
  if Assigned(FScripts) then begin
    for i := 0 to FScripts.Count - 1 do
      FScripts.Objects[i].Free;
    FScripts.Clear;
  end;
end;

function TConvertInfo.ColumnsToString(AColumns: Columns): string;
var
  i: Integer;
begin
  for i := 0 to AColumns.Count - 1 do
    Result := Result + GetColName(AColumns.Item[i].Name, False) + ',';
  Result := Copy(Result, 1, Length(Result) - 1);
end;

procedure TConvertInfo.ConnectToSourceCatalog;
var
  ConnectionString: string;
begin
  ConnectionString := FSourceConnectionString;
  FCatalog := CoCatalog.Create;
  try
    FCatalog._Set_ActiveConnection(ConnectionString);
  except
    FCatalog := nil;
    raise;
  end;
end;

constructor TConvertInfo.Create;
begin
  FScripts := TStringList.Create;
  FLibraries := TStringList.Create;
  FNameMap := TNameMap.Create(Self);
  FRestrictToTables := TStringList.Create;
  FDefaultValues := TStringList.Create;
end;

destructor TConvertInfo.Destroy;
begin
  ClearScripts;
  FLibraries.Free;
  FRestrictToTables.Free;
  FScripts.Free;
  FNameMap.Free;
  FDefaultValues.Free;
  inherited;
end;

procedure TConvertInfo.DisconnectCatalog;
begin
  FCatalog := nil;
end;

procedure TConvertInfo.Execute;
var
  MigrationManager: TMigrationManager;
begin
  MigrationManager := TMigrationManager.Create;
  try
    MigrationManager.OnExecutionFeedback := FOnExecutionFeedback;
    MigrationManager.Execute(Self);
    if Assigned(FOnShowErrors) then
      FOnShowErrors(Self, MigrationManager.Errors);
    if Assigned(FOnFinish) then
      FOnFinish(Self);
  finally
    MigrationManager.Free;
  end;
end;

procedure TConvertInfo.GenerateCheckScripts;
const
  SValidationRule = 'Jet OLEDB:Column Validation Rule';
var
  CheckClause: string;
  Column: _Column;
  ColumnName: string;
  ColIndex: Integer;
  Table: _Table;
  TableName: string;
  i: Integer;
  procedure AddCheckClause;
  var
    Script: TStrings;
  begin
    Script := TStringList.Create;
    Script.Add('ALTER TABLE ' + TableName + ' ');
    Script.Add('  ADD CONSTRAINT ' + GetColName(ColumnName, True) + 'Check');
    Script.Add('  CHECK (' + CheckClause + ')');
    FScripts.AddObject('CHECK: ' + TableName + ' (' +
      ColumnName + ')', Script);
  end;
  procedure FixCheckClause;
  var
    NewClause: string;
    InQuote: Boolean;
    P: PChar;
    QuoteChar: Char;
  begin
    { Validation rules in Access omit the name of the
      field. This should be included at the beginning of
      each clause, and after each AND or OR. If in dialect 3,
      we should change the quotation marks. }
    if FUseDialect3 then begin
      QuoteChar := '''';
      CheckClause := StringReplace(CheckClause, '"', QuoteChar,
        [rfReplaceAll]);
    end else
      QuoteChar := '"';
    CheckClause := GetColName(ColumnName, False) + ' ' + CheckClause;
    P := PChar(CheckClause);
    NewClause := '';
    InQuote := False;
    while P^ <> #0 do begin
      if P^ = QuoteChar then
        InQuote := not InQuote
      else if not InQuote then begin
        if AnsiStrLIComp(P, ' AND ', 5) = 0 then begin
          NewClause := NewClause + ' AND ' +
            GetColName(ColumnName, False) + ' ';
          Inc(P, 5);
          continue;
        end else if AnsiStrLIComp(P, ' OR ', 4) = 0 then begin
          NewClause := NewClause + ' OR ' +
            GetColName(ColumnName, False) + ' ';
          Inc(P, 4);
          continue;
        end;
      end;
      NewClause := NewClause + P^;
      Inc(P);
    end;
    CheckClause := NewClause;
  end;
begin
  for i := 0 to FCatalog.Tables.Count - 1 do begin
    Table := FCatalog.Tables.Item[i];
    if Table.Type_ <> 'TABLE' then
      continue;
    if not MustMigrateTable(Table.Name) then
      continue;
    if (FRestrictToTables.Count > 0) and
       (FRestrictToTables.IndexOf(Table.Name) = -1) then
      continue;
    TableName := GetTableName(Table.Name, False);
    for ColIndex := 0 to Table.Columns.Count - 1 do begin
      Column := Table.Columns[ColIndex];
      try
        CheckClause := Column.Properties.Item[SValidationRule].Value;
      except
        on E: EOleException do begin
          if (E.ErrorCode = PropertyNotFound) or
             (E.ErrorCode = IncapableProvider) then
            Exit;
          raise;
        end;
      end;
      if CheckClause <> '' then begin
        ColumnName := Column.Name;
        FixCheckClause;
        AddCheckClause;
      end;
    end;
  end;
end;

procedure TConvertInfo.GenerateDomainScripts;
begin
  if FCreateGUIDDomain then
    FScripts.AddObject('DOMAIN: GUID',
      CreateStrings('CREATE DOMAIN GUID CHAR(38)'));
end;

procedure TConvertInfo.GenerateGeneratorScripts;
var
  i, j: Integer;
  Col: Column;
  ColName: string;
  GenName: string;
  GenCount: Integer;
  Script: TStringList;
  Table: _Table;
  TableName: string;
  TableNameComposing: string;
begin
  { Generators are only created for autoinc columns (for now, anyway). }
  if not FMigrateAutoNumber then
    Exit;
  GenCount := 0;
  for i := 0 to FCatalog.Tables.Count - 1 do begin
    Table := FCatalog.Tables.Item[i];
    if Table.Type_ <> 'TABLE' then
      continue;
    if not MustMigrateTable(Table.Name) then
      continue;
    TableName := GetTableName(Table.Name, False);
    TableNameComposing := GetTableName(Table.Name, True);
    for j := 0 to Table.Columns.Count - 1 do begin
      Col := Table.Columns.Item[j];
      if not IsAutoInc(Table, Col) then
        continue;
      ColName := GetColName(Col.Name, True);
      Script := TStringList.Create;
      try
        GenName := Format(SGeneratorName,
          [TableNameComposing, ColName, GenCount]);
        Inc(GenCount);
        GenName := TruncateWithLeftTrim(GenName, GenNameMaxLen); 
        Script.Add('CREATE GENERATOR ' + GenName);
        FScripts.AddObject('GENERATOR: (' + TableName + ')', Script);

        { Initialize the generator, so we don't get a repeated number. }
        Script := TStringList.Create;
        Script.Add(Format(SInitGenerator,
          [GenName, MaxFieldValue(Table.Name, Col.Name) + 1]));
        FScripts.AddObject('GEN_INIT: (' + GenName + ')', Script);
      except
        Script.Free;
        raise;
      end;
    end;
  end;
end;

procedure TConvertInfo.GenerateIndexScripts;
var
  i, j: Integer;
  IndexCount: Integer;
  LastIndex: string;
  Table: _Table;
  TableName: string;
  procedure GenerateIndexScript(Index: _Index);
  var
    Columns: string;
    Line: string;
    IndexName: string;
    Strings: TStringList;
  begin
    Strings := TStringList.Create;
    if Index.PrimaryKey then begin
      try
        Index.Columns.Count;  { Read the value to test Columns. }
        Strings.Add('ALTER TABLE ' + TableName);
        Strings.Add('ADD PRIMARY KEY (');
        ListColumns(Index.Columns, Strings);
        Strings.Add(')');
      except
        { ODBC text drivers refuse to correctly populate the Columns
          collection. In this case, we skip this index. }
        on E: EOleException do begin
          if E.ErrorCode <> IncapableProvider then
            raise;
          Exit;
        end;
      end;
    end else begin
      { Access can have indexes in different tables with the same
        name, so we add a unique counter to distinguish them in
        InterBase. }
      Inc(IndexCount);
      IndexName := Index.Name + IntToStr(IndexCount);
      IndexName := TruncateWithLeftTrim(IndexName, IndexNameMaxLen);
      Line := 'CREATE ';
      if Index.Unique then
        Line := Line + 'UNIQUE ';
      { The problem about invalid index names was reported by
        John Reeve <jpr@ar5s.screaming.net>. }
      Strings.Add(Line + 'INDEX ' + GetIndexName(IndexName));
      Strings.Add('ON ' + TableName + '(');
      ListColumns(Index.Columns, Strings);
      Strings.Add(')');
    end;
    { Access will report primary keys as a unique index
      and as a primary key index. We use this to avoid
      including two identical definitions. }
    Columns := ColumnsToString(Index.Columns);
    if LastIndex = Columns then begin
      Strings.Free;
      Exit;
    end else
      LastIndex := Columns;
    FScripts.AddObject('INDEX (' + TableName + '): ' +
      Index.Name, Strings);
  end;
begin
  IndexCount := 0;
  for i := 0 to FCatalog.Tables.Count - 1 do begin
    Table := FCatalog.Tables.Item[i];
    if Table.Type_ <> 'TABLE' then
      continue;
    if not MustMigrateTable(Table.Name) then
      continue;
    TableName := GetTableName(Table.Name, False);
    for j := 0 to Table.Indexes.Count - 1 do
      GenerateIndexScript(Table.Indexes.Item[j]);
  end;
end;

function RuleToString(const Rule: RuleEnum): string;
const
  RuleText: array[adRINone..adRISetDefault] of string = (
    'NO ACTION', 'CASCADE', 'SET NULL', 'SET DEFAULT'
  );
begin
  Result := RuleText[Rule]; 
end;

procedure TConvertInfo.GenerateKeyScripts;
var
  AKey: Key;
  i, KeyIndex: Integer;
  Table: _Table;
  TableName: string;
  procedure GenerateKeyScript;
  var
    TableNameComposing: string;
    Script: TStringList;
  begin
    TableNameComposing := GetTableName(Table.Name, True);
    Script := TStringList.Create;
    try
      Script.Add('ALTER TABLE ' + TableName);
      Script.Add('ADD CONSTRAINT ' +
        Format(SKeyName, [TableNameComposing, KeyIndex]));
      Script.Add('FOREIGN KEY (');
      ListColumns(AKey.Columns, Script);
      Script.Add(') REFERENCES ' + GetTableName(AKey.RelatedTable, False) + '(');
      ListRelatedColumns(AKey.Columns, Script);
      Script.Add(')');
      if AKey.DeleteRule <> adRINone then
        Script.Add('ON DELETE ' + RuleToString(AKey.DeleteRule));
      if AKey.UpdateRule <> adRINone then
        Script.Add('ON UPDATE ' + RuleToString(AKey.DeleteRule));
      FScripts.AddObject('KEY: (' + TableName + ')', Script);
    except
      Script.Free;
      raise;
    end;
  end;
begin
  for i := 0 to FCatalog.Tables.Count - 1 do begin
    Table := FCatalog.Tables.Item[i];
    if Table.Type_ <> 'TABLE' then
      continue;
    if not MustMigrateTable(Table.Name) then
      continue;
    TableName := GetTableName(Table.Name, False);
    try
      for KeyIndex := 0 to Table.Keys.Count - 1 do begin
        AKey := Table.Keys.Item[KeyIndex];
        { Primary keys and unique keys are skipped here, because
          they are created as indexes and constraints. }
        if (AKey.Type_ = adKeyPrimary) or
           (AKey.Type_ = adKeyUnique) then
          continue;
        if AKey.Type_ = adKeyForeign then
          GenerateKeyScript
        else
          raise Exception.Create('Unknown key type (' +
            IntToStr(AKey.Type_));
      end;
    except
      on E: EOleException do begin
        if E.ErrorCode = IncapableProvider then
          Exit;
        raise;
      end;
    end;
  end;
end;

procedure TConvertInfo.GeneratePumpScripts;
var
  i: Integer;
  Script: TStringList;
  SrcTableName: string;
  Table: _Table;
  TableName: string;
begin
  for i := 0 to FCatalog.Tables.Count - 1 do begin
    Table := FCatalog.Tables.Item[i];
    if Table.Type_ <> 'TABLE' then
      continue;
    if not MustMigrateTable(Table.Name) then
      continue;
    SrcTableName := Table.Name;
    TableName := GetTableName(SrcTableName, False);
    Script := TStringList.Create;
    Script.Add('INSERT INTO ' + TableName + ' (');
    ListColumns(Table.Columns, Script);
    Script.Add(') VALUES (');
    ListPrefixColumns('  :', Table.Columns, Script);
    Script.Add(')');
    FScripts.AddObject('PUMP: (' + SrcTableName + ')', Script);
  end;
end;

procedure TConvertInfo.GenerateQueryScripts;
begin
  { TODO: Translate MS SQL stored procedure into InterBase stored procedures. }
  { Yeah, right! ;-) }
end;

procedure TConvertInfo.GenerateScripts;
begin
  CheckProvider;
  ClearScripts;
  IdentifierCount := 0;
  FNameMap.Clear;
  FNameMap.MapMethods := FMapMethods;
  ScriptFeedback('Connecting to source...');
  ConnectToSourceCatalog;
  try
    if not FSkipMetadata then begin
      ScriptFeedback('Generating UDF scripts...');
      GenerateUDFScripts;
      ScriptFeedback('Generating domain scripts...');
      GenerateDomainScripts;
      ScriptFeedback('Generating table scripts...');
      GenerateTableScripts;
    end;
    ScriptFeedback('Generating pumping scripts...');
    GeneratePumpScripts;
    if not FSkipMetadata then begin
      ScriptFeedback('Generating index scripts...');
      GenerateIndexScripts;
      ScriptFeedback('Generating check scripts...');
      GenerateCheckScripts;
      ScriptFeedback('Generating key scripts...');
      GenerateKeyScripts;
      ScriptFeedback('Generating generator scripts...');
      GenerateGeneratorScripts;
      if not FSkipTriggers then
        GenerateTriggerScripts;
      GenerateQueryScripts;
    end;
    ScriptFeedback('Scripts generated.');
  finally
    DisconnectCatalog;
  end;
end;

{ 2000.9.1: TODD added code to get the correct field order.
  2001.1.3: Marcelo changed to code to use ADO directly. }
procedure TConvertInfo.GenerateTableScripts;
var
  ColIndex: Integer;
  Column: _Column;
  DefaultQuery: Recordset;
  FieldsQuery: Recordset;
  FieldsQuerySQL: string;
  i, j: Integer;
  Line: string;
  Script: TStringList;
  Table: _Table;
  TableName: string;

  function ColumnIsFixed(Column: _Column): Boolean;
  begin
    Result := (Column.Attributes and adColFixed) <> 0;
  end;
  function ColumnToDataType(Column: _Column): string;
  begin
    case Column.Type_ of
      adBinary, adLongVarBinary:
        Result := 'BLOB';
      adBoolean:
        Result := 'CHAR(1)';
      adBSTR, adChar, adWChar:
        if ColumnIsFixed(Column) then
          Result := 'CHAR(' + IntToStr(Column.DefinedSize) + ')'
        else
          Result := 'VARCHAR(' + IntToStr(Column.DefinedSize) + ')';
      adVarChar, adVarWChar:
        Result := 'VARCHAR(' + IntToStr(Column.DefinedSize) + ')';
      adLongVarChar, adLongVarWChar:
        Result := 'BLOB SUB_TYPE TEXT';
      adDouble:
        Result := 'DOUBLE PRECISION';
      adCurrency:
        { Indicates a currency value (DBTYPE_CY). Currency is a
          fixed-point number with four digits to the right of the decimal
          point. It is stored in an eight-byte signed integer scaled by
          10,000. }
        Result := 'NUMERIC(18, 4)';
      adDate:
        if FUseDialect3 then
          Result := 'TIMESTAMP' else
          Result := 'DATE';
      adDBDate:
        Result := 'DATE';
      adDBTime:
        if FUseDialect3 then
          Result := 'TIME'
        else
          Result := 'DATE';
      adDBTimeStamp:
        if FUseDialect3 then
          Result := 'TIMESTAMP' else
          Result := 'DATE';
      adInteger:
        Result := 'INTEGER';
      adSingle:
        Result := 'FLOAT';
      adUnsignedTinyInt, { one byte, unsigned - IB's SMALLINT is even better }
      adSmallint, adTinyInt:
        Result := 'SMALLINT';
      adNumeric:
        Result := 'NUMERIC(' + IntToStr(Column.Precision) + ', ' +
           IntToStr(Column.NumericScale) + ')';
      adDecimal:
        Result := 'DECIMAL(' + IntToStr(Column.Precision) + ', ' +
          IntToStr(Column.NumericScale) + ')';
      adGuid:
        if FCreateGUIDDomain then
          Result := 'GUID'
        else
          Result := 'CHAR(38)';
      else
        raise Exception.CreateFmt(
          'Unsupported data type %s in column %s of table %s',
          [OleDbTypeToString(Column.Type_), Column.Name, Table.Name]);
    end;
  end;
  function ColumnToNullable(Column: _Column): string;
  begin
    if (Column.Attributes and adColNullable) = 0 then
      Result := ' NOT NULL' else
      Result := '' {'NULL'};
  end;

  { Retrieves the DEFAULT expression for an SQL Server column. }
  function ColumnToSQLServerDefault(Column: _Column): string;
  var
    DefaultQuerySQL: string;
    function MunchNextToken(var Str: string): string;
    const
      TokenSeparators = [' ', ',', ';', '.', #9, #13, #10];
    var
      i: Integer;
    begin
      i := 1;
      Result := '';
      while (i < Length(Str)) and
            (not (Str[i] in TokenSeparators)) do begin
        Result := Result + Str[i];
        Inc(i);
      end;
      Delete(Str, 1, i);
      Result := UpperCase(Result);
    end;
    function RemoveParenthesis(const Expression: string): string;
    begin
      { SQL Server has a love for parenthesis around default
        expressions. IB doesn't. }
      if (Length(Expression) > 1) and
         (Expression[1] = '(') and
         (Expression[Length(Expression)] = ')') then
        Result := RemoveParenthesis(
          Copy(Expression, 2, Length(Expression) - 2))
      else
        Result := Expression;
    end;
    function RemoveQuotes(const Expression: string): string;
    begin
      { Sometimes, depending on whether DEFAULTs were used in-place
        or not, extra quotes have to be removed - we'll insert our
        own as required, anyway. }
      if (Length(Expression) > 1) and
         (Expression[1] = '''') and
         (Expression[Length(Expression)] = '''') then
        Result := RemoveParenthesis(
          Copy(Expression, 2, Length(Expression) - 2))
      else
        Result := Expression;
    end;
  begin
    DefaultQuerySQL := 'select text ' +
      'from syscomments ' +
      'where id = (' +
      '  select cdefault ' +
      '  from syscolumns ' +
      '  where id = (' +
      '    select id ' +
      '    from sysobjects ' +
      '    where name = ' + QuotedStr(Table.Name) +
      '    and xtype = ''U''' +
      '  )' +
      '  and name = ' + QuotedStr(Column.Name) +
      ')';
    DefaultQuery.Open(DefaultQuerySQL, FSourceConnectionString,
      adOpenForwardOnly, adLockReadOnly, Integer(adOptionUnspecified));
    if not DefaultQuery.Eof then begin
      Result := VarToStr(DefaultQuery.Fields[0].Value);
      if Pos('AS', Result) > 0 then begin
        while (MunchNextToken(Result) <> 'AS') and
              (Length(Result) > 0 ) do;
        Result := Trim(Result);
      end else
        Result := Trim(RemoveParenthesis(Result));
      if Result <> '' then begin
        if UpperCase(Result) = 'GETDATE()' then
          Result := QuotedStr('NOW')
        else if FieldTypeIsString(Column.Type_) then
          Result := QuotedStr(RemoveQuotes(Result))
        else if not FieldTypeIsNumeric(Column.Type_) then
          Result := QuotedStr(Result);
        Result := ' DEFAULT ' + Result + ' ';
      end;
    end else
      Result := '';
    DefaultQuery.Close;
  end;
begin
  { Create fields in natural order. }
  FieldsQuery := CoRecordset.Create;
  DefaultQuery := CoRecordset.Create;

  for i := 0 to FCatalog.Tables.Count - 1 do begin
    Table := FCatalog.Tables.Item[i];
    if Table.Type_ < 'TABLE' then
      continue;
    if not MustMigrateTable(Table.Name) then
      continue;
    TableName := GetTableName(Table.Name, False);

    { Get field definitions, with not records. }
    ScriptFeedback('Generating script for table ' + Table.Name + '...');
    FieldsQuerySQL := 'SELECT * ' +
      'FROM ' + GetProviderTableName(Table.Name) + ' WHERE 0=1';
    FieldsQuery.Open(FieldsQuerySQL, FSourceConnectionString,
      adOpenForwardOnly, adLockReadOnly, Integer(adOptionUnspecified));

    Script := TStringList.Create;
    Script.Add('CREATE TABLE ' + TableName + ' (');

    { Put fields in the right order. }
    for j := 0 to FieldsQuery.Fields.Count - 1 do begin
      ColIndex := 0;

      repeat
        Column := Table.Columns.Item[ColIndex];
        Inc(ColIndex);
      until (Column.Name = FieldsQuery.Fields[j].Name);

      Line := '  ' + GetColName(Column.Name, False) + ' ' +
        ColumnToDataType(Column);
      if FOleDbProvider = opSQLServer then
        Line := Line + ColumnToSQLServerDefault(Column);
      Line := Line + ColumnToNullable(Column);

      if j < Table.Columns.Count - 1 then
        Line := Line + ',';

      Script.Add(Line);
    end;
    FieldsQuery.Close;

    Script.Add(')');
    FScripts.AddObject('TABLE: ' + TableName, Script);
  end;
  DefaultQuery := nil;
  FieldsQuery := nil;
end;

procedure TConvertInfo.GenerateTriggerScripts;
var
  i, j: Integer;
  Col: Column;
  GenName: string;
  GenCount: Integer;
  ColName: string;
  ColNameComposing: string;
  Script: TStringList;
  Table: _Table;
  TableName: string;
  TableNameComposing: string;
  TrigName: string;
begin
  { Triggers are (for now) only created for autoinc columns. }
  if not FMigrateAutoNumber then
    Exit;
  GenCount := 0;
  for i := 0 to FCatalog.Tables.Count - 1 do begin
    Table := FCatalog.Tables.Item[i];
    if Table.Type_ <> 'TABLE' then
      continue;
    if not MustMigrateTable(Table.Name) then
      continue;
    TableName := GetTableName(Table.Name, False);
    TableNameComposing := GetTableName(Table.Name, True);
    for j := 0 to Table.Columns.Count - 1 do begin
      Col := Table.Columns.Item[j];
      if not IsAutoInc(Table, Col) then
        continue;
      ColName := GetColName(Col.Name, False);
      ColNameComposing := GetColName(Col.Name, True);
      Script := TStringList.Create;
      try
        GenName := Format(SGeneratorName,
          [TableNameComposing, ColNameComposing, GenCount]);
        Inc(GenCount);
        GenName := TruncateWithLeftTrim(GenName, GenNameMaxLen);
        TrigName := TruncateWithLeftTrim('GetNext' + GenName, TrigNameMaxLen);
        Script.Add('CREATE TRIGGER ' + TrigName + ' FOR ' + TableName);
        Script.Add('BEFORE INSERT');
        Script.Add('AS');
        Script.Add('BEGIN');
        Script.Add('  NEW.' + ColName + ' = GEN_ID(' + GenName + ', 1);');
        Script.Add('END');
        FScripts.AddObject('TRIGGER: (' + TableName + ')', Script);
      except
        Script.Free;
        raise;
      end;
    end;
  end;
end;

procedure TConvertInfo.GenerateUDFScripts;
var
  DeclarationBody: string;
  i, j: Integer;
  Strings: TStringList;
  UDFCounter: Integer;
  function LibraryToFileName(const LibName: string): string;
  begin
    Result := ExtractFilePath(Application.ExeName) + LibName + LibraryEndMask;    
  end;
begin
  if FLibraries.Count = 0 then
    Exit;

  DeclarationBody := '';
  UDFCounter := 0;
  Strings := TStringList.Create;
  try
    for i := 0 to FLibraries.Count - 1 do begin
      Strings.LoadFromFile(LibraryToFileName(FLibraries[i]));
      for j := 0 to Strings.Count - 1 do begin
        DeclarationBody := DeclarationBody + Strings[j] + #13#10;
        if Pos(';', DeclarationBody) <> 0 then begin
          DeclarationBody := Trim(DeclarationBody);
          if Length(DeclarationBody) = 0 then
            continue;
          if DeclarationBody[Length(DeclarationBody)] <> ';' then
            raise Exception.Create(SIncorrectUDFTermination);
          Delete(DeclarationBody, Length(DeclarationBody), 1);
          Inc(UDFCounter);
          AddSimpleScript(Format(SUDFTitle, [FLibraries[i], UDFCounter]),
            DeclarationBody);
          DeclarationBody := '';
        end;
      end;
    end;
  finally
    Strings.Free;
  end;
end;

function TConvertInfo.GetColName(const ColumnName: string;
  const ForComposition: Boolean = True): string;
begin
  if (not ForComposition) and (mmUseQuoted in FMapMethods) then
    Result := Delimited(ColumnName)
  else
    Result := FNameMap.GetColName(ColumnName)
end;

function TConvertInfo.GetIndexName(const IndexName: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(IndexName) do
    if IndexName[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_'] then
      Result := Result + IndexName[i];
  if Result = '' then
    raise Exception.CreateFmt('Invalid index name %s for parsing', [IndexName]);
  if not (Result[1] in ['A'..'Z', 'a'..'z']) then
    Result := 'IX_' + Result;
end;

function TConvertInfo.GetProviderFieldName(const FieldName: string): string;
begin
  { Both MS SQL 7+ and Jet accept this syntax. }
  Result := '[' + FieldName + ']';
end;

function TConvertInfo.GetProviderTableName(
  const TableName: string): string;
begin
  { Both MS SQL and Jet accept this syntax. }
  Result := '[' + TableName + ']';
end;

function TConvertInfo.GetTableName(const TableName: string;
  const ForComposition: Boolean = True): string;
begin
  if (not ForComposition) and (mmUseQuoted in FMapMethods) then
    Result := Delimited(TableName)
  else
    Result := FNameMap.GetTableName(TableName);
end;

function TConvertInfo.IsAutoInc(ATable: Table; AColumn: Column): Boolean;
  function JetIsAutoInc(AColumn: Column): Boolean;
  var
    Prop: Property_;
  begin
    try
      //DumpProperties(AColumn.Properties);
      Prop := AColumn.Properties.Item[SAutoincrementing];
      Result := Assigned(Prop) and (Prop.Value = True);
    except
      Result := False;
    end;
  end;
  function SQLIsAutoInc(const TableName, ColumnName: string): Boolean;
  var
    CommandText: string;
    Rst: _Recordset;
  begin
    CommandText := 'SELECT Name ' +
      'FROM syscolumns ' +
      'WHERE (status & 128) = 128 ' +
      'AND Name=' + QuotedStr(ColumnName) + ' ' +
      'AND Id = (' +
      '  SELECT Id ' +
      '  FROM sysobjects ' +
      '  WHERE name=' + QuotedStr(TableName) + ' ' +
      '  AND type=''U''' +
      ')';
    Rst := OpenRecordset(CommandText);
    Result := not Rst.EOF;
  end;
begin
  case FOleDbProvider of
    opJet:
      Result := JetIsAutoInc(AColumn);
    opSQLServer:
      Result := SQLIsAutoInc(ATable.Name, AColumn.Name);
    else
      Result := False;
  end;
end;

function TConvertInfo.IsReservedWord(Token: string): Boolean;
begin
  Result := FNameMap.IsReservedWord(Token);
end;

procedure TConvertInfo.ListColumns(AColumns: Columns; Strings: TStrings);
begin
  ListPrefixColumns('  ', AColumns, Strings);
end;

procedure TConvertInfo.ListPrefixColumns(const Prefix: string;
  AColumns: Columns; Strings: TStrings);
var
  i: Integer;
  Line: string;
begin
  for i := 0 to AColumns.Count - 1 do begin
    Line := Prefix + GetColName(AColumns.Item[i].Name, False);
    if i < AColumns.Count - 1 then
      Line := Line + ',';
    Strings.Add(Line);
  end;
end;

procedure TConvertInfo.ListPrefixRelatedColumns(const Prefix: string;
  AColumns: Columns; Strings: TStrings);
var
  i: Integer;
  Line: string;
begin
  for i := 0 to AColumns.Count - 1 do begin
    Line := Prefix + GetColName(AColumns.Item[i].RelatedColumn, False);
    if i < AColumns.Count - 1 then
      Line := Line + ',';
    Strings.Add(Line);
  end;
end;

procedure TConvertInfo.ListRelatedColumns(AColumns: Columns;
  Strings: TStrings);
begin
  ListPrefixRelatedColumns('  ', AColumns, Strings);
end;

procedure TConvertInfo.LoadFromScript(const FileName: string);
var
  Line: string;
  Strings: TStringList;
  ScriptName: string;
  ScriptBody: TStringList;
  function StrStarts(Line: string; const Marker: string;
    var Value: string): Boolean;
  begin
    if Copy(Line, 1, Length(Marker)) = Marker then begin
      Result := True;
      Value := Copy(Line, Length(Marker) + 2, Length(Line));
    end else
      Result := False;
  end;
  procedure LoadLine(Line: string);
  var
    Value: string;
  begin
    if StrStarts(Line, SCreateGUIDDomain, Value) then
      CreateGUIDDomain := StrToBool(Value)
    else if StrStarts(Line, SCharSet, Value) then
      CharSet := Value
    else if StrStarts(Line, SNameMap, Value) then
      NameMap.LoadFromString(Value)
    else if StrStarts(Line, SDeleteExisting, Value) then
      DeleteExisting := StrToBool(Value)
    else if StrStarts(Line, SDefaultValues, Value) then
      DefaultValues.CommaText := Value
    else if StrStarts(Line, SUseExisting, Value) then
      UseExisting := StrToBool(Value)
    else if StrStarts(Line, SInterruptOnErrors, Value) then
      InterruptOnErrors := StrToBool(Value)
    else if StrStarts(Line, SIBFileName, Value) then
      IBFileName := Value
    else if StrStarts(Line, SIBPassword, Value) then
      IBPassword := Value
    else if StrStarts(Line, SIBUserName, Value) then
      IBUserName := Value
    else if StrStarts(Line, SMapMethods, Value) then
      SetSetProp(Self, 'MapMethods', Value)
    else if StrStarts(Line, SMigrateAutoNumber, Value) then
      MigrateAutoNumber := StrToBool(Value)
    else if StrStarts(Line, SOutputFileName, Value) then
      OutputFileName := Value
    else if StrStarts(Line, SOutputType, Value) then
      SetEnumProp(Self, 'OutputType', Value)
    else if StrStarts(Line, SRestrictToTables, Value) then
      RestrictToTables.CommaText := Value
    else if StrStarts(Line, SLibraries, Value) then
      Libraries.CommaText := Value
    else if StrStarts(Line, SSkipMetadata, Value) then
      SkipMetadata := StrToBool(Value)
    else if StrStarts(Line, SSourceConnectionString, Value) then
      SourceConnectionString := LineToStr(Value)
    else if StrStarts(Line, SUseDialect3, Value) then
      UseDialect3 := StrToBool(Value)
    else if StrStarts(Line, SUseExisting, Value) then
      UseExisting := StrToBool(Value)
    else if StrStarts(Line, SSkipTriggers, Value) then
      SkipTriggers := StrToBool(Value)
    else
      raise Exception.CreateFmt(SUnknownWizardMarker, [Line]);
  end;
  procedure RemoveTrailing(Strings: TStrings; const AChar: Char);
  var
    Text: string;
  begin
    Text := Trim(Strings.Text);
    if (Length(Text) > 0) and (Text[Length(Text)] = AChar) then
      Strings.Text := Copy(Text, 1, Length(Text)-1);
  end;
begin
  ScriptBody := nil;
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(FileName);
    { Remove the top comments. }
    while (Strings.Count > 0) and (Strings[0] <> SWizardInfoStart) do
      Strings.Delete(0);
    if Strings.Count = 0 then
      raise Exception.Create(SNoWizardStartMarker);

    { Read the 'markers' - that is, migration properties. }
    Strings.Delete(0);
      Line := Strings[0];
    while (Line <> SWizardInfoStop) and
          (Strings.Count > 0) do begin
      LoadLine(Line);
      Strings.Delete(0);
      if Strings.Count > 0 then
        Line := Strings[0];
    end;
    if Line = SWizardInfoStop then
      Strings.Delete(0);

    { Anything after the marker is parsed as scripts. }
    while Strings.Count > 0 do begin
      Line := Strings[0];
      if ScriptName = '' then begin
        ScriptName := Trim(RemoveCommentTags(Line));
        Strings.Delete(0);
      end else begin
        if not Assigned(ScriptBody) then
          ScriptBody := TStringList.Create;
        { This *really* could be improved. }
        if (Pos('/*', Line) > 0) and
           (Pos('*/', Line) > 0) and
           (Pos(':', Line) > 0) then begin
          RemoveTrailing(ScriptBody, ';');
          Scripts.AddObject(ScriptName, ScriptBody);
          ScriptName := '';
          ScriptBody := nil;
        end else begin
          ScriptBody.Add(Line);
          Strings.Delete(0);
        end;
      end;
    end;
    if Assigned(ScriptBody) then
      Scripts.AddObject(ScriptName, ScriptBody);
  finally
    Strings.Free;
  end;
end;

function TConvertInfo.MaxFieldValue(const TableName,
  FieldName: string): Integer;
var
  CommandText: string;
  Fld: Field;
  Rst: _Recordset;
begin
  { Fix suggested by Woody [woody.tmw@ih2000.net]. }
  CommandText := 'SELECT MAX(' + GetProviderFieldName(FieldName) + ') FROM ' +
    TableName;
  Rst := OpenRecordset(CommandText);
  if Rst.EOF then
    Result := 0
  else begin
    { It has been reported that the correct value will
      not always be returned when the recordset is empty. Odd. }
    Fld := Rst.Fields.Item[0];
    if VarIsNull(Fld.Value) then
      Result := 0 else
      Result := Fld.Value;
  end;
end;

function TConvertInfo.MustMigrateTable(const TableName: string): Boolean;
begin
  if (FRestrictToTables.Count = 0) or
     (FRestrictToTables.IndexOf(TableName) <> -1) then
    Result := True else
    Result := False;
end;

function TConvertInfo.OpenRecordset(const CommandText: string): _Recordset;
var
  ConnDisp: IDispatch;
  Conn: _Connection;
  Affected: OleVariant;
begin
  ConnDisp := FCatalog.Get_ActiveConnection;
  Conn := ConnDisp as _Connection;
  Result := Conn.Execute(CommandText, Affected, 0);
end;

procedure TConvertInfo.SaveToScript(const FileName: string);
var
  i: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Add('/* MS SQL to InterBase Wizard Script */');
    Strings.Add('/* Generation moment: ' + DateTimeToStr(Now) + ' */');
    Strings.Add('/* The following section allows the wizard to');
    Strings.Add(' *  reload all the migration information later on.');
    Strings.Add(' *  Do not modify by hand, unless you *know* what');
    Strings.Add(' *  doing. */');
    Strings.Add(SWizardInfoStart);
    Strings.Add(SCreateGUIDDomain + '=' + BoolToStr(CreateGUIDDomain));
    Strings.Add(SCharSet + '=' + CharSet);
    Strings.Add(SDefaultValues + '=' + DefaultValues.CommaText);
    Strings.Add(SDeleteExisting + '=' + BoolToStr(DeleteExisting));
    Strings.Add(SIBFileName + '=' + IBFileName);
    Strings.Add(SIBPassword + '=' + IBPassword);
    Strings.Add(SIBUserName + '=' + IBUserName);
    Strings.Add(SInterruptOnErrors + '=' + BoolToStr(InterruptOnErrors));
    Strings.Add(SNameMap + '=' + FNameMap.SaveToString);
    Strings.Add(SMapMethods + '=' + GetSetProp(Self, 'MapMethods', True));
    Strings.Add(SMigrateAutoNumber + '=' + BoolToStr(MigrateAutoNumber));
    Strings.Add(SOutputFileName + '=' + OutputFileName);
    Strings.Add(SOutputType + '=' + GetEnumProp(Self, 'OutputType'));
    Strings.Add(SRestrictToTables + '=' + RestrictToTables.CommaText);
    Strings.Add(SLibraries + '=' + Libraries.CommaText);
    Strings.Add(SSkipMetadata + '=' + BoolToStr(SkipMetadata));
    Strings.Add(SSkipTriggers + '=' + BoolToStr(SkipTriggers));
    Strings.Add(SSourceConnectionString + '=' + StrToLine(SourceConnectionString));
    Strings.Add(SUseDialect3 + '=' + BoolToStr(UseDialect3));
    Strings.Add(SUseExisting + '=' + BoolToStr(UseExisting));
    Strings.Add(SWizardInfoStop);
    for i := 0 to Scripts.Count - 1  do begin
      Strings.Add('');
      Strings.Add('/* ' + Scripts[i] + ' */');
      Strings.AddStrings(TStrings(Scripts.Objects[i]));
      Strings.Add(';');
    end;
    Strings.SaveToFile(FileName);
  finally
    Strings.Free;
  end;
end;

procedure TConvertInfo.ScriptFeedback(const Text: string);
begin
  if Assigned(FOnScriptingFeedback) then
    FOnScriptingFeedback(Self, Text);
end;

procedure TConvertInfo.SetLibraries(const Value: TStrings);
begin
  FLibraries.Assign(Value);
end;

procedure TConvertInfo.SetDefaultValues(Value: TStrings);
begin
  FDefaultValues.Assign(Value);
end;

procedure TConvertInfo.SetRestrictToTables(const Value: TStrings);
begin
  FRestrictToTables.Assign(Value);
end;

function TConvertInfo.UnmangeColumnName(const ColumnName: string): string;
begin
  Result := FNameMap.UnmangeColumnName(ColumnName);
end;

function TConvertInfo.UnmangleTableName(const TableName: string): string;
begin
  Result := FNameMap.UnmangleTableName(TableName);
end;

{ TNameMap }

procedure TNameMap.Clear;
begin
  FSrcToDestCol.Clear;
  FSrcToDestTable.Clear;
  FDestToSrcCol.Clear;
  FDestToSrcTable.Clear;
end;

constructor TNameMap.Create(ConvertInfo: TConvertInfo);
begin
  FConvertInfo := ConvertInfo;
  FMapMethods := MapMethodIB56;

  FSrcToDestCol := TSimpleMap.Create;
  FDestToSrcCol := TSimpleMap.Create;
  FSrcToDestTable := TSimpleMap.Create;
  FDestToSrcTable := TSimpleMap.Create;
  FReservedWords := TStringList.Create;

  LoadReservedWords;
end;

destructor TNameMap.Destroy;
begin
  FSrcToDestCol.Free;
  FDestToSrcCol.Free;
  FSrcToDestTable.Free;
  FDestToSrcTable.Free;
  inherited;
end;

function TNameMap.GetColName(const ColumnName: string): string;
begin
  Result := FSrcToDestCol.Values[ColumnName];
  if Result = '' then begin
    Result := MangleColumnName(ColumnName);
    FSrcToDestCol.Values[ColumnName] := Result;
    FDestToSrcCol.Values[Result] := ColumnName;
  end;
end;

function TNameMap.GetTableName(const TableName: string): string;
begin
  Result := FSrcToDestTable.Values[TableName];
  if Result = '' then begin
    Result := MangleTableName(TableName);
    FSrcToDestTable.Values[TableName] := Result;
    FDestToSrcTable.Values[Result] := TableName;
  end;
end;

function TNameMap.IsReservedWord(Token: string): Boolean;
var
  Index: Integer;
begin
  Token := UpperCase(Token);
  Result := FReservedWords.Find(Token, Index);
end;

function TNameMap.IsSimpleIdentifier(const Token: string): Boolean;
var
  P: PChar;
begin
  P := PChar(Token);
  if not (P^ in AlphaChars) then begin
    Result := False;
    Exit;
  end;
  while P^ <> #0 do begin
    if not (P^ in SimpleChars) then begin
      Result := False;
      exit;
    end;
    Inc(P);
  end;
  Result := True;
end;

procedure TNameMap.LoadFromString(const S: string);
var
  List: TStringList;
begin
  FSrcToDestCol.Clear;
  FDestToSrcCol.Clear;
  FSrcToDestTable.Clear;
  FDestToSrcTable.Clear;
  List := TStringList.Create;
  try
    List.CommaText := S;
    if List.Count = 0 then Exit;
    FSrcToDestCol.CommaText := List[0];
    if List.Count = 1 then Exit;
    FDestToSrcCol.CommaText := List[1];
    if List.Count = 2 then Exit;
    FSrcToDestTable.CommaText := List[2];
    if List.Count = 3 then Exit;
    FDestToSrcTable.CommaText := List[3];
  finally
    List.Free;
  end;
end;

function TNameMap.SaveToString: string;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Add(FSrcToDestCol.CommaText);
    List.Add(FDestToSrcCol.CommaText);
    List.Add(FSrcToDestTable.CommaText);
    List.Add(FDestToSrcTable.CommaText);
    Result := List.CommaText;
  finally
    List.Free;
  end;
end;

procedure TNameMap.LoadReservedWords;
var
  FileName: string;
begin
  FileName := ExtractFilePath(Application.ExeName) + 'reserved.txt';
  if not FileExists(FileName) then
    ShowMessageFmt(SReservedWordsMissing, [FileName])
  else begin
    FReservedWords.LoadFromFile(FileName);
    FReservedWords.Sorted := True;
  end;
end;

function TNameMap.MangleColumnName(ColumnName: string): string;
begin
  Result := MangleIdentifier(ColumnName, SColSuffix);
end;

function ReplaceUnfriendlyIdChars(const Text, ReplaceWith: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Text) do
    if Text[i] in FriendlyChars then
      Result := Result + Text[i]
    else
      Result := Result + ReplaceWith;
  { First character of table and column names must be alphabetic. }
  if Result = '' then begin
    Result := GetIdentifierPrefix(True)
  end else if not (Result[1] in AlphaChars) then
    Result := GetIdentifierPrefix(False) + Result;
end;

function TNameMap.MangleIdentifier(const Identifier,
  Suffix: string): string;
begin
  if IsSimpleIdentifier(Identifier) then
    Result := Identifier
  else if mmUseQuoted in FMapMethods then
    Result := QuotedStr(Identifier)
  else if mmUnderscores in FMapMethods then
    Result := ReplaceUnfriendlyIdChars(Identifier, '_')
  else if mmRemoveWhiteSpace in FMapMethods then
    Result := ReplaceUnfriendlyIdChars(Identifier, '');
  if not (mmUseQuoted in FMapMethods) then begin
    if IsReservedWord(Result) then begin
      Result := Result + Suffix;
      FDestToSrcCol.Values[Identifier] := Result;
    end;
  end;
end;

function TNameMap.MangleTableName(const TableName: string): string;
begin
  Result := MangleIdentifier(TableName, STableSuffix);
end;

function TNameMap.UnmangeColumnName(const ColumnName: string): string;
begin
  Result := FDestToSrcCol.Values[ColumnName];
  if Result = '' then
    Result := ColumnName;
end;

function TNameMap.UnmangleTableName(const TableName: string): string;
begin
  Result := FDestToSrcTable.Values[TableName];
  if Result = '' then
    Result := TableName;
end;

end.
