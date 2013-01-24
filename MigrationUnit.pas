{*****************************************************************************}
{                                                                             }
{ Migration Library                                                           }
{ Provides classes to perform the migration from ADO to InterBase once        }
{ all the required information has been gathered.                             }
{                                                                             }
{*****************************************************************************}

unit MigrationUnit;

interface

uses
  ADODB_TLB, Classes, ConvertInfoUnit, IBDatabase, IBQuery, 
  SQL2GDBConstsUnit, SysUtils;

type

{ TMigrationManager is responsible for executing the migration scripts
  according to the information gathered.

  Execute             - Executes the migration. Use ConvertInfo to specify
                        the migration information.
  Errors              - After execution, this property holds all the errors
                        found during the migration
  OnExecutionFeedback - This is called whenever there is a change of state
                        the user might be interested in. }
  TMigrationManager = class
  private
    FAccessConn: _Connection;
    FConvertInfo: TConvertInfo;
    FErrors: TStrings;
    FIBDatabase: TIBDatabase;
    FIBQuery: TIBQuery;
    FIBTx: TIBTransaction;
    FOnExecutionFeedback: TFeedbackEvent;
    FOutputFile: TextFile;
    procedure ConnectToDatabases;
    procedure CreateChecks;
    procedure CreateDomains;
    procedure CreateGenerators;
    procedure CreateIBDatabase;
    procedure CreateIndexes;
    procedure CreateKeys;
    procedure CreateTables;
    procedure CreateTriggers;
    procedure CreateUDFs;
    procedure DialectDowngrade(Sender: TObject);
    procedure DisconnectFromDatabases;
    procedure ExecuteScripts(const StartingWith: string);
    procedure ExecutionFeedback(const Text: string);
    function GetDefaultFieldValue(const TableName, FieldName: string; 
      out Value: string): Boolean;
    procedure PumpData;
    procedure ReportError(const Text: string);
    procedure WriteOutputHeader;
    procedure WriteRstOut(const TableName: string; Query: TStrings; 
      Rst: _Recordset);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(ConvertInfo: TConvertInfo);

    property Errors: TStrings read FErrors;
    property OnExecutionFeedback: TFeedbackEvent read FOnExecutionFeedback write FOnExecutionFeedback;
  end;

  EDialectDowngrade = class(Exception);

implementation

uses
  Base64Unit, ComObj, Db, Dialogs, FileCtrl, Forms;

const
  CRLF = #13#10;
  CRLF2 = #13#10#13#10;


  { Indexes into array: (param is boolean?, field value being mapped) }
  BooleanParamValues: array[Boolean, Boolean] of string = (
    ('F', 'T'),
    ('0', '1'){
      AParam.Value := BooleanParamValues[AParam.DataType = ftBoolean, FieldValue];
      if FieldValue = True then
        AParam.Value := 'T'
      else
        AParam.Value := 'F';
      if AParam.DataType = ftBoolean then AParam.Value := AParam.AsString[1] = 'T';}

  );

  SDialectDownGrade = 'The database does not support the requested dialect.';
  SFalseConstant = '''F''';
  STrueConstant = '''T''';

  SPreciseDateTimeFormat = 'yyyy-mm-dd hh:nn:ss:zzz';

function DatabaseNameIsLocal(const FileName: string): Boolean;
begin
  { We cannot be sure, but we can guess: if it has a colon on the
    second position, then it's probably a local drive. }
  Result := (Length(FileName) > 2) and (FileName[2] = ':');
end;

procedure ForceFileDirectories(const FileName: string);
var
  Dir: string;
begin
  Dir := ExtractFileDir(FileName);
  if (Dir <> '') and not (DirectoryExists(Dir)) then
    ForceDirectories(Dir);
end;

{ TMigrationManager }

procedure TMigrationManager.ConnectToDatabases;
var
  ConnectionString: WideString;
begin
  ExecutionFeedback('Connecting to databases...');

  if FConvertInfo.OutputType = otIB then begin
    if FConvertInfo.UseDialect3 then
      FIBDatabase.SQLDialect := 3
    else
      FIBDatabase.SQLDialect := 1;
    FIBDatabase.DatabaseName := FConvertInfo.IBFileName;
    FIBDatabase.Params.Add('user_name=' + FConvertInfo.IBUserName);
    FIBDatabase.Params.Add('password=' + FConvertInfo.IBPassword);
    if FConvertInfo.CharSet <> '' then
      FIBDatabase.Params.Add('lc_ctype=' + FConvertInfo.CharSet);
    FIBDatabase.LoginPrompt := False;
    FIBDatabase.OnDialectDowngradeWarning := DialectDowngrade;
    FIBDatabase.Open;
  end else begin
    AssignFile(FOutputFile, FConvertInfo.OutputFileName);
    Rewrite(FOutputFile);
    WriteOutputHeader;
  end;

  ConnectionString := FConvertInfo.SourceConnectionString;
  FAccessConn := CoConnection.Create;
  FAccessConn.Open(ConnectionString, '', '', 0);
end;

constructor TMigrationManager.Create;
begin
  FIBDatabase := TIBDatabase.Create(nil);
  FIBTx := TIBTransaction.Create(nil);
  FIBQuery := TIBQuery.Create(nil);
  FErrors := TStringList.Create;

  FIBDatabase.DefaultTransaction := FIBTx;
  FIBTx.DefaultDatabase := FIBDatabase;
  FIBQuery.Database := FIBDatabase;
  FIBQuery.Transaction := FIBTx;
end;

procedure TMigrationManager.CreateChecks;
begin
  ExecutionFeedback('Creating checks...');
  ExecuteScripts('CHECK');
end;

procedure TMigrationManager.CreateDomains;
begin
  ExecutionFeedback('Creating generators...');
  ExecuteScripts('DOMAIN');
end;

procedure TMigrationManager.CreateGenerators;
begin
  ExecutionFeedback('Creating generators...');
  ExecuteScripts('GENERATOR');
  ExecutionFeedback('Initializing generators...');
  ExecuteScripts('GEN_INIT');
end;

procedure TMigrationManager.CreateIBDatabase;
begin
  ExecutionFeedback('Creating InterBase database...');

  if FileExists(FConvertInfo.IBFileName) and
     FConvertInfo.DeleteExisting then
    DeleteFile(FConvertInfo.IBFileName);

  FIBDatabase.DatabaseName := FConvertInfo.IBFileName;
  FIBDatabase.Params.Add('USER ' + QuotedStr(FConvertInfo.IBUserName));
  FIBDatabase.Params.Add('PASSWORD ' + QuotedStr(FConvertInfo.IBPassword));
  FIBDatabase.Params.Add('PAGE_SIZE = 4096');
  if FConvertInfo.CharSet <> '' then
    FIBDatabase.Params.Add('DEFAULT CHARACTER SET ' + FConvertInfo.CharSet);
  if FConvertInfo.UseDialect3 then
    FIBDatabase.SQLDialect := 3 else
    FIBDatabase.SQLDialect := 1;
  if DatabaseNameIsLocal(FIBDatabase.DatabaseName) then
    ForceFileDirectories(FIBDatabase.DatabaseName);
  FIBDatabase.CreateDatabase;
  FIBDatabase.Close;
  FIBDatabase.Params.Clear;
end;

procedure TMigrationManager.CreateIndexes;
begin
  ExecutionFeedback('Creating indexes...');
  ExecuteScripts('INDEX');
end;

procedure TMigrationManager.CreateKeys;
begin
  ExecutionFeedback('Creating keys...');
  ExecuteScripts('KEY');
end;

procedure TMigrationManager.CreateTables;
begin
  ExecutionFeedback('Creating tables...');
  ExecuteScripts('TABLE');
end;

procedure TMigrationManager.CreateTriggers;
begin
  ExecutionFeedback('Creating triggers...');
  ExecuteScripts('TRIGGER');
end;

procedure TMigrationManager.CreateUDFs;
begin
  ExecutionFeedback('Declaring UDFs...');
  ExecuteScripts('UDF');
end;

destructor TMigrationManager.Destroy;
begin
  FIBDatabase.Free;
  FIBTx.Free;
  FIBQuery.Free;
  FErrors.Free;
  inherited;
end;

procedure TMigrationManager.DialectDowngrade(Sender: TObject);
begin
  raise EDialectDowngrade.Create(SDialectDownGrade);
end;

procedure TMigrationManager.DisconnectFromDatabases;
begin
  FAccessConn := nil;
  if FConvertInfo.OutputType = otIB then
    FIBDatabase.Connected := False
  else if FConvertInfo.OutputType = otText then
    CloseFile(FOutputFile);
end;

procedure TMigrationManager.Execute(ConvertInfo: TConvertInfo);
begin
  FConvertInfo := ConvertInfo;
  if (FConvertInfo.OutputType = otIB) and
     (not FConvertInfo.UseExisting) then
    CreateIBDatabase;
  ConnectToDatabases;
  try
    CreateUDFs;
    CreateDomains;
    CreateTables;
    PumpData;
    CreateIndexes;
    CreateChecks;
    CreateGenerators;
    CreateTriggers;
    CreateKeys;
  finally
    DisconnectFromDatabases;
  end;
end;

procedure TMigrationManager.ExecuteScripts(const StartingWith: string);
var
  i: Integer;
  Scripts: TStrings;
  StartLen: Integer;
  TrimmedQuery: string;
begin
  Scripts := FConvertInfo.Scripts;
  StartLen := Length(StartingWith);

  for i := 0 to Scripts.Count - 1 do begin
    if Copy(Scripts[i], 1, StartLen) <> StartingWith then
      continue;
    FIBQuery.SQL := TStrings(Scripts.Objects[i]);
    TrimmedQuery := Trim(FIBQuery.SQL.Text);
    if (TrimmedQuery = '') or (TrimmedQuery = ';') then
      Continue;
    try
      if FConvertInfo.OutputType = otIB then
        FIBQuery.ExecSQL
      else if FConvertInfo.OutputType = otText then
        Writeln(FOutputFile, FIBQuery.SQL.Text, ';');
    except
      on E: Exception do begin
        ReportError(
          'Executing query ' + Scripts[i] + ' with text ' + CRLF +
          TStrings(Scripts.Objects[i]).Text + CRLF2 +
          'Produced error ' +
          E.Message);
      end;
    end;
  end;
end;

procedure TMigrationManager.ExecutionFeedback(const Text: string);
begin
  if Assigned(FOnExecutionFeedback) then
    FOnExecutionFeedback(Self, Text);
end;

function TMigrationManager.GetDefaultFieldValue(
  const TableName, FieldName: string; out Value: string): Boolean;
  function DefaultFound(const ValueName: string; var Value: string;
    var Found: Boolean): Boolean;
  begin
    Found := FConvertInfo.DefaultValues.IndexOfName(ValueName) <> -1;
    Result := Found;
    if Result then Value := FConvertInfo.DefaultValues.Values[ValueName];
  end;
begin
  Result := False;
  Value := '';
  if FConvertInfo.DefaultValues.Count = 0 then Exit;
  if DefaultFound(TableName + '.' + FieldName, Value, Result) then Exit;
  if DefaultFound('*.' + FieldName, Value, Result) then Exit;
  if DefaultFound(TableName + '.*', Value, Result) then Exit;
  if DefaultFound(FieldName, Value, Result) then Exit;
  Value := FConvertInfo.DefaultValues.Values[TableName + '.' + FieldName];
end;

procedure TMigrationManager.PumpData;
const
//  CommitGranularity = 10000;
//  CommitedMessage = '0K records commited.';
  CommitGranularity = 1000;
  CommitedMessageFmt = '%dK records commited for %s.';
  FieldNotFound = -2146825023;
  StartingWith = 'PUMP: (';
var
  CommandText: WideString;
  i,j: Integer;
  RecordsAffected: OleVariant;
  Rst: _Recordset;
  Scripts: TStrings;
  StartLen: Integer;
  TableName: string;
  ThRecsCommited: Integer;  { Records commited, in thousands. }

  function ListRstValues: string;
  var
    i: Integer;
  begin
    if Rst = nil then
      Result := 'No Recordset Assigned'
    else begin
      Result := 'Values: ';
      for i := 0 to Rst.Fields.Count - 1 do
        if FieldIsBlob(Rst.Fields[i]) then
          Result := Result + Rst.Fields[i].Name + ' [Blob]; '
        else if VarIsNull(Rst.Fields[i].Value) then
          Result := Result + Rst.Fields[i].Name + ' [Null]; '
        else
          Result := Result + Rst.Fields[i].Name + ' [' +
            VarToStr(Rst.Fields[i].Value) + ']; ';
    end;
  end;
  procedure AssignValue(AField: Field; AParam: TParam);
  var
    DefValue: string;
    FieldValue: Variant;
  begin
    { Manage special cases. }
    FieldValue := AField.Value;
    if VarIsNull(FieldValue) and
       GetDefaultFieldValue(TableName, AField.Name, DefValue) then begin
      FieldValue := DefValue;
      if AParam.DataType = ftBoolean then begin
        if Length(DefValue) = 0 then
          FieldValue := False
        else if UpCase(DefValue[1]) = 'F' then
          FieldValue := False
        else
          FieldValue := True;
      end;
    end;
    if AField.Type_ = adBoolean then
      AParam.Value := BooleanParamValues[AParam.DataType = ftBoolean, FieldValue = True]
    else
      AParam.Value := FieldValue;
  end;
  function FieldNames: string;
  var i: Integer;
  begin
    Result := '';
    for i := 0 to Rst.Fields.Count - 1 do begin
      if i > 0 then Result := Result + ',';
      Result := Result + Rst.Fields.Item[i].Name;
    end;
  end;
  procedure MapParams;
  var
    AField: Field;
    ColName: string;
    i: Integer;
    Param: TParam;
    Params: TParams;
  begin
    Params := FIBQuery.Params;
    for i := 0 to Params.Count - 1 do begin
      Param := Params[i];
      ColName := FConvertInfo.UnmangeColumnName(Param.Name);
      try
        if Param.DataType = ftUnknown then begin
          Param.DataType := OleDbTypeToFieldType(Rst.Fields.Item[ColName].Type_);
          { ftWideString is not supported by IBX parameters. }
          if Param.DataType = ftWideString then
            Param.DataType := ftString;
        end;
        AField := Rst.Fields.Item[ColName];
      except
        on E: Exception do begin
          raise Exception.CreateFmt(
            'Error retrieving column %s from collection %s ' +
            'with parameter %s '#13#10'%s: %s',
            [ColName, FieldNames, Param.Name, E.ClassName, E.Message]);
        end;
      end;
      AssignValue(AField, Param);
    end;
  end;
  procedure PumpRst;
  begin
    j := 0;
    if not FIBTx.InTransaction then
      FIBTx.StartTransaction;
    try
      ThRecsCommited := 0;
      while not Rst.EOF do begin
        { Commit transaction after 10,000 records. }
        inc(j);
        if j = CommitGranularity then begin
          if FIBTx.InTransaction then
            FIBTx.Commit;
          if not FIBTx.InTransaction then
            FIBTx.StartTransaction;
          Inc(ThRecsCommited);
          ExecutionFeedback(
            Format(CommitedMessageFmt, [ThRecsCommited, TableName]));
          j := 0;
        end;
        try
          MapParams;
          FIBQuery.ExecSQL;
        except
          on E: Exception do begin
            ReportError(
              'Executing query ' + Scripts[i] + ' with text ' + CRLF +
              TStrings(Scripts.Objects[i]).Text + CRLF2 +
              'With Recordset: ' + CRLF +
              ListRstValues + CRLF2 +
              'Produced error ' + CRLF +
              E.Message + ' (' + E.ClassName + ')');
            FIBQuery.UnPrepare;
          end;
        end;
        Rst.MoveNext;
      end;
    except
      if FIBTx.InTransaction then
        FIBTx.Rollback;
    end;

    if FIBTx.InTransaction then
      FIBTx.Commit;
  end;

  procedure PatchBlobFields;
  var
    i: Integer;
    AField: Field;
    Param: TParam;
  begin
    { The IBX component do not detect a generic blob correctly
      in the parameters. These are set from the field definitions. }
    for i := 0 to Rst.Fields.Count - 1 do begin
      AField := Rst.Fields.Item[i];
      if not FieldIsBlob(AField) then
        continue;
      Param := FIBQuery.Params.ParamByName(FConvertInfo.GetColName(AField.Name));
      Param.DataType := ftBlob;
    end;
  end;
begin
  ExecutionFeedback('Moving data...');
  Scripts := FConvertInfo.Scripts;
  StartLen := Length(StartingWith);

  for i := 0 to Scripts.Count - 1 do begin
    if Copy(Scripts[i], 1, StartLen) <> StartingWith then
      continue;

    TableName := Copy(Scripts[i], StartLen + 1, 1024);
    TableName := Copy(TableName, 1, Length(TableName) - 1);
    if Pos(' ', TableName) > 0 then
      TableName := '[' + TableName + ']';
    CommandText := 'SELECT * FROM ' + TableName;

    FIBQuery.SQL := TStrings(Scripts.Objects[i]);
    try
      Rst := FAccessConn.Execute(CommandText, RecordsAffected, 0);
      if FConvertInfo.OutputType = otIB then begin
        FIBQuery.Prepare;
        try
          PatchBlobFields;
          PumpRst;
        finally
          FIBQuery.UnPrepare;
          Rst := nil;
        end;
      end else if FConvertInfo.OutputType = otText then begin
        WriteRstOut(TableName, FIBQuery.SQL, Rst);
      end;
    except
      on E: Exception do begin
        ReportError(
          'Executing query ' + Scripts[i] + ' with text '#13 +
          TStrings(Scripts.Objects[i]).Text +
          #13#13'Produced error ' +
          E.Message);
      end;
    end;
  end;
end;

procedure TMigrationManager.ReportError(const Text: string);
begin
  if FConvertInfo.InterruptOnErrors then
    raise Exception.Create(Text)
  else begin
    FErrors.Add('------');
    FErrors.Add(Text);
    FErrors.Add(' ');
  end;
end;

procedure TMigrationManager.WriteOutputHeader;
var
  FileName: string;
  Strings: TStringList;
begin
  Assert(FConvertInfo.OutputType = otText);
  FileName := ExtractFilePath(Application.ExeName) + 'scripthead.sql';
  if not FileExists(FileName) then
    Exit;
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(FileName);
    Writeln(FOutputFile, Strings.Text);
  finally
    Strings.Free;
  end;
end;

procedure TMigrationManager.WriteRstOut(const TableName: string;
  Query: TStrings; Rst: _Recordset);
var
  AField: Field;
  ColName: string;
  i: Integer;
  ParamName: string;
  ParamPos: Integer;
  QueryText: string;
  function FieldAsString(AField: Field): string;
  begin
    if VarIsNull(AField.Value) then begin
      if GetDefaultFieldValue(TableName, AField.Name, Result) then
        Result := QuotedStr(Result)
      else
        Result := 'NULL';
      Exit;
    end;
    case AField.Type_ of
      adBoolean:
        if AField.Value = True then
          Result := STrueConstant
        else
          Result := SFalseConstant;
      adTinyInt, adSmallInt, adInteger,
      adBigInt, adUnsignedTinyInt, adUnsignedSmallInt,
      adUnsignedInt, adUnsignedBigInt, adSingle,
      adDouble, adCurrency, adDecimal,
      adNumeric:
        Result := AField.Value;
      adBinary, adVarBinary, adLongVarBinary:
        // Inserting as base64 does not work. Future enhancement?
        //Result := QuotedStr(StringToBase64(VarToStr(AField.Value)));
        Result := 'NULL /* binary field */';
      adLongVarChar, adLongVarWChar:
        Result := 'NULL /* text field */';
      adDBTimeStamp:
        { This includes more information than the
          default formatting does. }
        Result := QuotedStr(FormatDateTime(SPreciseDateTimeFormat,
          AField.Value));
      else
        Result := QuotedStr(AField.Value);
    end;
  end;
begin
  while not Rst.Eof do begin
    QueryText := Query.Text;
    for i := 0 to Rst.Fields.Count - 1 do begin
      AField := Rst.Fields.Item[i];
      ColName := FConvertInfo.GetColName(AField.Name, False);
      ParamName := ':' + ColName;
      ParamPos := Pos(ParamName, QueryText);
      if ParamPos > 0 then begin
        Delete(QueryText, ParamPos, Length(ParamName));
        Insert(FieldAsString(AField), QueryText, ParamPos);
      end;
    end;
    Writeln(FOutputFile);
    Writeln(FOutputFile, QueryText, ';');
    Rst.MoveNext;
  end;
end;

end.
