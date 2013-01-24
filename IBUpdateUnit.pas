{ IBUpdateUnit.pas
  This unit manages update documents.
  For a description of the file format used, see IBUpdate.htm. }
unit IBUpdateUnit;

interface

uses Classes, SysUtils;

type
  TIBUpdateTopic = class;

  TIBUpdateDocument = class
  private
    FMajorVersion: Integer;
    FMinorVersion: Integer;
    FLastUpdate: TDateTime;
    FTopics: TList;
    function GetTopicCount: Integer;
    function GetTopics(Index: Integer): TIBUpdateTopic;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Topic: TIBUpdateTopic);
    procedure Clear;
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);
    function TopicByName(const TopicName: string): TIBUpdateTopic;

    property MajorVersion: Integer read FMajorVersion write FMajorVersion;
    property MinorVersion: Integer read FMinorVersion write FMinorVersion;
    property LastUpdate: TDateTime read FLastUpdate write FLastUpdate;
    property Topics[Index: Integer]: TIBUpdateTopic read GetTopics;
    property TopicCount: Integer read GetTopicCount;
  end;

  TIBUpdateTopic = class
  private
    FDescription: string;
    FName: string;
    FLastUpdate: TDateTime;
    FBody: TStrings;
    procedure SetBody(const Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    property Body: TStrings read FBody write SetBody;
    property Description: string read FDescription write FDescription;
    property Name: string read FName write FName;
    property LastUpdate: TDateTime read FLastUpdate write FLastUpdate;
  end;

function GetStdDate(StdDate: string): TDateTime;
function MakeStdDate(const Value: TDateTime): string;

implementation

const
  MaxMajorVersion = 1;
  SUnknownVersion = 'Unknown update version %d.';

function MakeStdDate(const Value: TDateTime): string;
begin
  Result := FormatDateTime('yyyy.mm.dd.hh.nn', Value);
end;

function MunchInteger(var S: string): Integer;
var
  DotPos: Integer;
begin
  DotPos := Pos('.', S);
  if DotPos = 0 then begin
    Result := StrToInt(S);
    S := '';
  end else begin
    Result := StrToInt(Copy(S, 1, DotPos - 1));
    Delete(S, 1, DotPos);
  end;
end;

procedure GetMajorMinor(var MajorVersion, MinorVersion: Integer;
  const S: String);
begin
  MajorVersion := StrToInt(Copy(S, 1, Pos('.', S) - 1));
  MinorVersion := StrToInt(Copy(S, Pos('.', S) + 1, 255));
end;

function GetStdDate(StdDate: string): TDateTime;
var
  Year, Month, Day, Hour, Min: Word;
begin
  Year := MunchInteger(StdDate);
  Month := MunchInteger(StdDate);
  Day := MunchInteger(StdDate);
  Hour := MunchInteger(StdDate);
  Min := MunchInteger(StdDate);
  Result := EncodeDate(Year, Month, Day) +
            EncodeTime(Hour, Min, 0, 0);
end;

{ TIBUpdateDocument }

procedure TIBUpdateDocument.Add(Topic: TIBUpdateTopic);
begin
  FTopics.Add(Topic);  
end;

procedure TIBUpdateDocument.Clear;
var
  i: Integer;
begin
  if not Assigned(FTopics) then
    Exit;
  for i := 0 to FTopics.Count - 1 do
    TObject(FTopics[i]).Free;
  FTopics.Clear;
end;

constructor TIBUpdateDocument.Create;
begin
  FTopics := TList.Create;
  FMajorVersion := MaxMajorVersion;
  FMinorVersion := 0;
end;

destructor TIBUpdateDocument.Destroy;
begin
  Clear;
  FTopics.Free;
  inherited;
end;

function TIBUpdateDocument.GetTopicCount: Integer;
begin
  Result := FTopics.Count;
end;

function TIBUpdateDocument.GetTopics(Index: Integer): TIBUpdateTopic;
begin
  Result := TIBUpdateTopic(FTopics[Index]);
end;

procedure TIBUpdateDocument.LoadFromStrings(Strings: TStrings);
var
  Topic: TIBUpdateTopic;
  TopicsLeft: Integer;
  function GetNextLine: string;
  begin
    Result := Strings[0];
    Strings.Delete(0);
  end;
  function NoMoreLines: Boolean;
  begin
    Result := Strings.Count = 0;
  end;
  function LoadNewTopic: TIBUpdateTopic;
  var
    BodyCount: Integer;
  begin
    Result := TIBUpdateTopic.Create;
    Result.Name := GetNextLine;
    Result.Description := GetNextLine;
    Result.LastUpdate := GetStdDate(GetNextLine);
    BodyCount := StrToInt(GetNextLine);
    while BodyCount > 0 do begin
      Result.Body.Add(GetNextLine);
      Dec(BodyCount);
    end;
  end;
begin
  Clear;
  GetMajorMinor(FMajorVersion, FMinorVersion, GetNextLine);
  if FMajorVersion > MaxMajorVersion then
    raise Exception.CreateFmt(SUnknownVersion, [FMajorVersion]);
  FLastUpdate := GetStdDate(GetNextLine);
  if NoMoreLines then Exit;
  TopicsLeft := StrToInt(GetNextLine);
  while TopicsLeft > 0 do begin
    Topic := LoadNewTopic;
    FTopics.Add(Topic);
    Dec(TopicsLeft);
  end;
end;

procedure TIBUpdateDocument.SaveToStrings(Strings: TStrings);
var
  i: Integer;
begin
  Strings.Add(Format('%d.%d', [FMajorVersion, FMinorVersion]));
  Strings.Add(MakeStdDate(FLastUpdate));
  Strings.Add(IntToStr(FTopics.Count));
  for i := 0 to FTopics.Count - 1 do begin
    Strings.Add(Topics[i].Name);
    Strings.Add(Topics[i].Description);
    Strings.Add(MakeStdDate(Topics[i].LastUpdate));
    Strings.Add(IntToStr(Topics[i].Body.Count));
    Strings.AddStrings(Topics[i].Body);
  end;
end;

function TIBUpdateDocument.TopicByName(
  const TopicName: string): TIBUpdateTopic;
var
  i: Integer;
begin
  for i := 0 to FTopics.Count - 1 do begin
    Result := Topics[i];
    if AnsiCompareText(Result.Name, TopicName) = 0 then
      Exit;
  end;
  Result := nil;
end;

{ TIBUpdateTopic }

constructor TIBUpdateTopic.Create;
begin
  FBody := TStringList.Create;
end;

destructor TIBUpdateTopic.Destroy;
begin
  FBody.Free;
  inherited;
end;

procedure TIBUpdateTopic.SetBody(const Value: TStrings);
begin
  FBody.Assign(Value);
end;

end.
