{*****************************************************************************}
{                                                                             }
{ Text Model Unit                                                             }
{ Provides classes to manage the creation of text schema files.               }
{                                                                             }
{*****************************************************************************}

unit TextModelUnit;

interface

uses
  Classes, SysUtils;

type
  TTextWizardStep = (twsTextPreview, twsTextDelimiter, twsTextFields);

{ TTextInfo provides the information required to build a schema.ini file. }
  TDataLayout = (dlDelimited, dlFixedWidth);
  TFieldSeparator = (fsSpace, fsTab, fsComma, fsSemiColon, fsDash);
  TFieldSeparatorSet = set of TFieldSeparator;

const
  FieldSeparatorChars: array[TFieldSeparator] of Char = (
    ' ', #9, ',', ';', '-'
  );

type

{
  Schema.ini file format information can be found in the following URL.
  http://msdn.microsoft.com/library/default.asp?url=/library/en-us/odbc/htm/odbcjetschema_ini_file.asp

  DataLayout        - indicates fixed-width or delimited fields
  FirstRowHasNames  - indicates whether the first row has field names
  SchemaFileName    - file name for the schema.ini file
  SchemaColumns     - column descriptions
  Separator         - the field separator for delimited fields
  TableName         - table name for the text file
  TextFileName      - file name of the text file to import
}

{$TYPEINFO ON}
  TTextInfo = class
  private
    FDataLayout: TDataLayout;
    FGuess: Boolean;
    FFirstRowHasNames: Boolean;
    FSchemaColumns: TStringList;
    FSchemaFileName: string;
    FSeparator: TFieldSeparator;
    FTextFileName: string;
    function GetTableName: string;
    procedure SetTextFileName(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GuessFromFileData;
    procedure GuessFromFileName;
    procedure LoadSchemaFromFile;
    procedure SaveSchemaToFile;
  published
    property DataLayout: TDataLayout read FDataLayout write FDataLayout;
    property FirstRowHasNames: Boolean read FFirstRowHasNames write FFirstRowHasNames;
    property Guess: Boolean read FGuess;
    property SchemaFileName: string read FSchemaFileName;
    property SchemaColumns: TStringList read FSchemaColumns;
    property Separator: TFieldSeparator read FSeparator write FSeparator;
    property TableName: string read GetTableName;
    property TextFileName: string read FTextFileName write SetTextFileName;
  end;
{$TYPEINFO OFF}

{ ITextInfoSupport is implemented by objects that can keep a reference
  to a TTextInfo instance. }

  ITextInfoSupport = interface
    ['{E9162A04-4F17-453E-991B-CEAD437C8B19}']
    function GetTextInfo: TTextInfo;
    procedure SetTextInfo(Value: TTextInfo);
    property TextInfo: TTextInfo read GetTextInfo write SetTextInfo;
  end;

type
  TColInfo = record
    Name: string;
    FieldType: string;
    Width: Integer;
    Extras: string;
  end;

function ColInfoToStr(const ColInfo: TColInfo; out ColInfoLine: string): string;
function StrToColInfo(const ColInfoLine: string): TColInfo;
function StrToColName(const ColName: string): string;

implementation

uses
  Db, FileCtrl, IniFiles;

type
  TTextInfoGuesser = class
  private
    FieldNames: array of string; 
    FieldLengths: array of Integer;
    FieldTypes: array of TFieldType;
    FCols: TStrings;
    FInfo: TTextInfo;
    FMaxLinesExplored: Integer;
  protected
    procedure CheckFieldLengths(Values: TStrings);
    procedure GuessFieldDefs(Values: TStrings);
    procedure ReadLineValues(const Line: string; Strings: TStrings);
    procedure SetupInitialProperties;
    function StrToFieldName(const FieldName: string): string;
  public
    constructor Create;
    procedure Execute;
    property Info: TTextInfo read FInfo write FInfo;
    property MaxLinesExplored: Integer read FMaxLinesExplored write FMaxLinesExplored;
  end;

resourcestring
  sSchemaFileMissing = 'Schema file %s not found';
  sUnsupportedDelimiter = 'Unsupported delimiter specified: %s';

function ColInfoToStr(const ColInfo: TColInfo; out ColInfoLine: string): string;
begin
  Result := '';
  if ColInfo.Name <> '' then
    Result := Result + ColInfo.Name;
  if ColInfo.FieldType <> '' then
    Result := Result + ' ' + ColInfo.FieldType;
  if ColInfo.Width > 0 then
    Result := Result + ' Width ' + IntToStr(ColInfo.Width);
  if ColInfo.Extras <> '' then
    Result := Result + ColInfo.Extras;
end;

function StrToColInfo(const ColInfoLine: string): TColInfo;
const
  WhiteSpace = [' ', #9];
var
  P: PChar;
  S: string;
  function GetToken: string;
  begin
    Result := '';
    while (P^ <> #0) and (P^ in WhiteSpace) do Inc(P);
    if P^ = '"' then begin
      Result := '"';
      Inc(P);
      while (P^ <> #0) and (P^ <> '"') do begin
        Result := Result + P^;
        Inc(P);
      end;
      if P^ <> #0 then Inc(P);
      Result := Result + '"';
      Exit;
    end;
    while (P^ <> #0) and not (P^ in WhiteSpace) do begin
      Result := Result + P^;
      Inc(P);
    end;
  end;
begin
  with Result do begin
    Name := '';
    FieldType := '';
    Width := 0;
    Extras := '';
  end;
  P := PChar(ColInfoLine);
  S := GetToken;
  if S <> '' then begin
    Result.Name := S;
    S := GetToken;
    if S <> '' then begin
      Result.FieldType := S;
      S := GetToken;
      while S <> '' do begin
        if (S = 'Width') then begin
          S := GetToken;
          Result.Width := StrToIntDef(S, 0);
        end else
          Result.Extras := Result.Extras + S;
        S := GetToken;
      end;
    end;
  end;
end;

function StrToColName(const ColName: string): string;
begin
  Result := Trim(ColName);
  if Pos(' ', Result) > 0 then
    Result := AnsiQuotedStr(Result, '"');
end;

function Starts(const AString, SubString: string): Boolean;
begin
  Result := Copy(AString, 1, Length(SubString)) = SubString;
end;

{ TTextInfo }

constructor TTextInfo.Create;
begin
  inherited;
  FSchemaColumns := TStringList.Create;
end;

destructor TTextInfo.Destroy;
begin
  FSchemaColumns.Free;
  inherited;
end;

procedure TTextInfo.SetTextFileName(const Value: String);
begin
  FTextFileName := Value;
  FSchemaFileName := ExtractFilePath(Value) + 'Schema.ini';
end;

procedure TTextInfo.SaveSchemaToFile;
var
  i: Integer;
  IniFile: TIniFile;
  IniFormat: string;
begin
  IniFile := TIniFile.Create(SchemaFileName);
  try
    IniFile.EraseSection(TableName);
    IniFormat := '';
    if DataLayout = dlDelimited then begin
      case Separator of
        fsTab:
          IniFormat := 'TabDelimited';
        fsComma:
          IniFormat := 'CSVDelimited';
        fsSpace:
          IniFormat := 'Delimited( )';
        fsSemiColon:
          IniFormat := 'Delimited(;)';
        fsDash:
          IniFormat := 'Delimited(-)';
      end;
    end else begin
      IniFormat := 'FixedLength';
    end;
    IniFile.WriteString(TableName, 'Format', IniFormat);
    if FirstRowHasNames then
      IniFile.WriteString(TableName, 'FirstRowHasNames', 'True');
    for i := 0 to SchemaColumns.Count - 1 do begin
      IniFile.WriteString(TableName, 'Col' + IntToStr(i + 1),
        SchemaColumns[i]);
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TTextInfo.GuessFromFileData;
var
  Guesser: TTextInfoGuesser;
begin
  Guesser := TTextInfoGuesser.Create;
  try
    Guesser.Info := Self;
    Guesser.Execute;
  finally
    Guesser.Free;
  end;
end;

procedure TTextInfo.LoadSchemaFromFile;
var
  i: Integer;
  ColSpec: string;
  IniFile: TIniFile;
  IniFormat: string;
begin
  if not FileExists(SchemaFileName) then
    raise Exception.CreateFmt(sSchemaFileMissing, [SchemaFileName]);
  FGuess := False;
  IniFile := TIniFile.Create(SchemaFileName);
  try
    IniFormat := LowerCase(IniFile.ReadString(TableName, 'Format', ''));
    if IniFormat = 'tabdelimited' then begin
      DataLayout := dlDelimited;
      Separator := fsTab;
    end else if IniFormat = 'csvdelimited' then begin
      DataLayout := dlDelimited;
      Separator := fsComma;
    end else if Starts(IniFormat, 'delimited(') then begin
      DataLayout := dlDelimited;
      case IniFormat[11] of
        ',':  Separator := fsComma;
        '-':  Separator := fsDash;
        ';':  Separator := fsSemiColon;
        #9:   Separator := fsTab;
        ' ':  Separator := fsSpace;
        else  raise Exception.CreateFmt(sUnsupportedDelimiter, [IniFormat]);
      end;
    end else if IniFormat = 'fixedlength' then
      DataLayout := dlFixedWidth;
    FirstRowHasNames := 'true' =
      LowerCase(IniFile.ReadString(TableName, 'FirstRowHasNames', 'True'));
    i := 1;
    repeat
      ColSpec := IniFile.ReadString(TableName, 'Col' + IntToStr(i), '');
      if ColSpec <> '' then begin
        SchemaColumns.Add(ColSpec);
        Inc(i);
      end;
    until ColSpec = '';
  finally
    IniFile.Free;
  end;
end;

procedure TTextInfo.GuessFromFileName;
var
  Ext: string;
begin
  FGuess := True;
  Ext := LowerCase(ExtractFileExt(TextFileName));
  if Ext = '.txt' then begin
    DataLayout := dlFixedWidth;
    Separator := fsTab;
  end else if Ext = '.csv' then begin
    DataLayout := dlDelimited;
    Separator := fsComma;
  end else if Ext = '.tsv' then begin
    DataLayout := dlDelimited;
    Separator := fsTab;
  end else begin
    DataLayout := dlFixedWidth;
    Separator := fsComma;
  end;
  FirstRowHasNames := True;
end;

function TTextInfo.GetTableName: string;
begin
  Result := ExtractFileName(TextFileName);
end;

{ TTextInfoGuesser }

{
Guessing a field format uses the following algorithm.

First, we start with an unknown field type. Certain hints will place
us in another very specific field type. Then, different hints will add
flexibility to the field type.

The hints are as follows:
- a "True" or "False" value found
- characters found
- numbers and a period or comma found
- numbers and a "/" or "-" found
- numbers a "-" found

The following table is used
ftUnknown
- True/False    ->  Bit       [ftBoolean]
- Character     ->  Char      [ftString]
- Nrs & Minus   ->  Integer   [ftInteger]
- Nrs & Period  ->  Float     [ftFloat]
- Nrs & Dash    ->  Date      [ftDateTime]

ftBoolean
- not "True/False"  -> Char

ftChar  - always stays as char

ftInteger
- Chars         -> Char
- Nrs & Period  -> Float

ftFloat
- Chars         -> Char

ftDateTime
- Chars         -> Char
}

type
  TFieldEvaluation = (
    feTrueFalse,    { True / False literals }
    feNotTrueFalse, { Anything but a True / False literal }
    feChars,        { Alphabetical characters }
    feNPerDash,     { Numerics, a period and a dash }
    feNDateLike,    { Numerics, periods, dashes and colons, looks like date }
    feNDash         { Numerics and dashes }
  );
  TEvaluationEntry = record
    Curr: TFieldType;
    Ev: TFieldEvaluation;
    Next: TFieldType;
  end;
const
  { Note: order is important, e.g., feTrueFalse is a subset of feChars. }
  EvaluationTable: array[0..10] of TEvaluationEntry = (
    (Curr:ftUnknown;  Ev: feTrueFalse;    Next: ftBoolean;),
    (Curr:ftUnknown;  Ev: feChars;        Next: ftString;),
    (Curr:ftUnknown;  Ev: feNDateLike;    Next: ftDateTime;),
    (Curr:ftUnknown;  Ev: feNDash;        Next: ftInteger;),
    (Curr:ftUnknown;  Ev: feNPerDash;     Next: ftFloat;),

    (Curr:ftBoolean;  Ev: feNotTrueFalse; Next: ftString;),

    (Curr:ftInteger;  Ev: feChars;        Next: ftString;),
    (Curr:ftInteger;  Ev: feNDash;        Next: ftInteger;),
    (Curr:ftInteger;  Ev: feNPerDash;     Next: ftFloat;),

    (Curr:ftFloat;    Ev: feChars;        Next: ftFloat;),

    (Curr:ftDateTime; Ev: feChars;        Next: ftString)
  );

type
  TCharSet = set of Char;

function ExclusiveInSet(const S: string; const CharSet: TCharSet): Boolean;
var
  P: PChar;
begin
  Result := False;
  P := PChar(S);
  while P^ <> #0 do begin
    if not (P^ in CharSet) then
      Exit;
    Inc(P);
  end;
  Result := True;
end;

function CountChars(const S: string; const C: Char): Integer;
var
  P: PChar;
begin
  Result := 0;
  P := PChar(S);
  while P^ <> #0 do begin
    if P^ = C then Inc(Result);
    Inc(P);
  end;
end;

function Eval(S: string; Kind: TFieldEvaluation): Boolean;
const
  Numerics: set of char = ['0'..'9'];
begin
  { Blank values may be null, and we really cannot know anything
    about them. }
  Result := False;
  if S = '' then Exit;
  case Kind of
    feTrueFalse: begin
      { True / False literals }
      if (LowerCase(S) = 'true') or (LowerCase(S) = 'false') then
        Result := True;
    end;
    feNotTrueFalse: begin
      { Anything but a True / False literal }
      if (LowerCase(S) <> 'true') and (LowerCase(S) <> 'false') then
        Result := True;
    end;
    feChars: begin
      { Alphabetical characters }
      Result := not ExclusiveInSet(S, Numerics + [' ', '.', '-']);
    end;
    feNPerDash: begin
      { Numerics, a period and a dash }
      Result := ExclusiveInSet(S, Numerics + ['.', '-', ' ']);
      if Result then
        Result := (CountChars(S, '.') <= 1) and (CountChars(S, '-') <= 1);
    end;
    feNDateLike: begin
      { Numerics, periods, dashes and colons, looks like date }
      Result := ExclusiveInSet(S, Numerics + ['.', '-', ' ']);
      if Result then { minimum yymmdd }
        Result := Length(S) > 5;
      { TODO: check that it looks like a date }
    end;
    feNDash: begin
      { Numerics and dashes }
      Result := ExclusiveInSet(S, Numerics + ['-', ' ']);
    end;
  end;
end;

function FieldToSchema(FieldType: TFieldType): string;
begin
  case FieldType of
    ftBoolean:  Result := 'Bit';
    ftInteger:  Result := 'Integer';
    ftString:   Result := 'Char';
    ftDateTime: Result := 'DateTime';
    ftFloat:    Result := 'Float';
    else        Result := 'Char';
  end;
end;

constructor TTextInfoGuesser.Create;
begin
  FMaxLinesExplored := 64;
end;

procedure TTextInfoGuesser.CheckFieldLengths(Values: TStrings);
var
  i: Integer;
begin
  for i := 0 to FCols.Count - 1 do
    if Length(Values[i]) > FieldLengths[i] then
      FieldLengths[i] := Length(Values[i]);
end;

procedure TTextInfoGuesser.GuessFieldDefs(Values: TStrings);
var
  i, j: Integer;
begin
  for i := 0 to FCols.Count - 1 do begin
    for j := Low(EvaluationTable) to High(EvaluationTable) do begin
      if (FieldTypes[i] = EvaluationTable[j].Curr) and
         Eval(Values[i], EvaluationTable[j].Ev) then begin
        FieldTypes[i] := EvaluationTable[j].Next;
        break;
      end;
    end;
  end;
end;

procedure TTextInfoGuesser.ReadLineValues(const Line: string; Strings: TStrings);
var
  C: Char;
  ColInfo: TColInfo;
  I: Integer;
  P, PStart: PChar;
  CopyPos: Integer;
begin
  Strings.Clear;
  if Info.DataLayout = dlDelimited then begin
    C := FieldSeparatorChars[Info.Separator];
    P := PChar(Line);
    PStart := P;
    while P^ <> #0 do begin
      if P^ = C then begin
        P^ := #0;
        Strings.Add(StrPas(PStart));
        PStart := P + 1;
      end;
      Inc(P);
    end;
    if PStart < P then
      Strings.Add(StrPas(PStart));
  end else begin
    CopyPos := 1;
    for i := 0 to FCols.Count - 1 do begin
      ColInfo := StrToColInfo(FCols[i]);
      if ColInfo.Width > 0 then begin
        Strings.Add(Copy(Line, CopyPos, ColInfo.Width));
        Inc(CopyPos, ColInfo.Width);
      end;
    end;
  end;
end;

procedure TTextInfoGuesser.Execute;
var
  F: TextFile;
  Line: string;
  LinesExplored: Integer;
  i: Integer;
  Strings: TStringList;
begin
  { This method will guess the field definitions from the text file. }
  FCols := Info.SchemaColumns;
  Info.FGuess := True;
  LinesExplored := 0;
  Strings := nil;
  AssignFile(F, Info.TextFileName);
  Reset(F);
  try
    Strings := TStringList.Create;
    if Eof(F) then Exit;
    Readln(F, Line);
    ReadLineValues(Line, Strings);
    
    { Set up the names for the fields. }
    SetLength(FieldNames, Strings.Count);
    if Info.FirstRowHasNames then begin
      for i := 0 to Strings.Count - 1 do
        FieldNames[i] := StrToFieldName(Strings[i]);
      if Eof(F) then Exit;
      Readln(F, Line);
      ReadLineValues(Line, Strings);
    end else begin
      for i := 0 to Strings.Count - 1 do
        FieldNames[i] := 'Col' + IntToStr(i + 1);
    end;

    { Setup initial properties to explore. }
    SetupInitialProperties;

    { Explore lines guessing field types and checking lengths. }
    repeat
      GuessFieldDefs(Strings);
      if Info.DataLayout = dlDelimited then
        CheckFieldLengths(Strings);
      Inc(LinesExplored);
      if Eof(F) then
        Line := ''
      else begin
        Readln(F, Line);
        ReadLineValues(Line, Strings);
      end;
    until (Line = '') or (LinesExplored > MaxLinesExplored);
    
    { Rebuild the schema columns. }
    for i := 0 to FCols.Count - 1 do begin
      FCols[i] := FieldNames[i] + ' ' + FieldToSchema(FieldTypes[i]);
      if FieldLengths[i] > 0 then
        FCols[i] := FCols[i] + ' Width ' + IntToStr(FieldLengths[i]);
    end;
  finally
    CloseFile(F);
    Strings.Free;
  end;
end;

procedure TTextInfoGuesser.SetupInitialProperties;
var
  ColInfo: TColInfo;
  i: Integer;
begin
  SetLength(FieldTypes, Length(FieldNames));
  SetLength(FieldLengths, Length(FieldNames));
  for i := 0 to FCols.Count - 1 do begin
    FieldTypes[i] := ftUnknown;
    if Info.DataLayout = dlDelimited then
      FieldLengths[i] := 0
    else begin
      ColInfo := StrToColInfo(FCols[i]);
      FieldLengths[i] := ColInfo.Width;
    end;
  end;
end;

function TTextInfoGuesser.StrToFieldName(const FieldName: string): string;
begin
  Result := StrToColName(FieldName);
end;

end.
