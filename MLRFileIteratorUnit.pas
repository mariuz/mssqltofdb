unit MLRFileIteratorUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TFileAttribute = (faReadOnly, faHidden, faSysFile, faVolumeID, faDirectory,
    faArchive);
  TFileAttributes = set of TFileAttribute;
  TBeforeIteratingEvent = procedure (Sender :TObject; Files :TStrings) of object;
  TFileFoundEvent = procedure (Sender :TObject; FileName :string;
    var Continue :Boolean) of object;

  TMLRFileIterator = class(TComponent)
  private
    FAttributes: TFileAttributes;
    FBeforeIterating: TBeforeIteratingEvent;
    FFileFoundEvent: TFileFoundEvent;
    FFilter: string;
    FFolder: string;
    FIncludeFolder: Boolean;
    FUseAttributes: Boolean;
    function GetFullFilter: string;
    procedure SetFilter(const Value: string);
    procedure SetFolder(const Value: string);
    procedure SetFullFilter(const Value: string);
  protected
    procedure DoBeforeIterating(FileNames: TStrings); virtual;
    procedure DoFileFound(const FileName: string; var Continue: Boolean); virtual;
  public
    { Public declarations }
    constructor Create(AOwner :TComponent); override;
    procedure Execute;
    procedure ListFiles(Strings :TStrings);
    property FullFilter :string read GetFullFilter write SetFullFilter;
  published
    { Published declarations }
    property Attributes: TFileAttributes read FAttributes write FAttributes default [];
    property Filter: string read FFilter write SetFilter;
    property Folder: string read FFolder write SetFolder;
    property IncludeFolder: Boolean read FIncludeFolder write FIncludeFolder default True;
    property UseAttributes: Boolean read FUseAttributes write FUseAttributes default False;
    property OnBeforeIterating: TBeforeIteratingEvent read FBeforeIterating write FBeforeIterating;
    property OnFileFound: TFileFoundEvent read FFileFoundEvent write FFileFoundEvent;
  end;

procedure ListFiles(Strings: TStrings; Folder: string;
  Filter: string = '*.*';
  IncludeFolder: Boolean = True;
  Attributes: TFileAttributes = []);
procedure Register;

implementation

procedure ListFiles(Strings: TStrings; Folder, Filter: string;
  IncludeFolder: Boolean; Attributes: TFileAttributes);
var
  Iterator: TMLRFileIterator;
begin
  Iterator := TMLRFileIterator.Create(nil);
  try
    Iterator.Folder := Folder;
    Iterator.Filter := Filter;
    Iterator.IncludeFolder := IncludeFolder;
    if Attributes = [] then
      Iterator.UseAttributes := False
    else begin
      Iterator.Attributes := Attributes;
      Iterator.UseAttributes := True;
    end;
    Iterator.ListFiles(Strings);
  finally
    Iterator.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TMLRFileIterator]);
end;

{ TMLRFileIterator }

function Slash(const s :string) :string;
begin
  Result := s;
  if s = '' then exit;
  if s[Length(s)] = '\' then exit;
  Result := s + '\';
end;

constructor TMLRFileIterator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilter := '*.*';
  FAttributes := [];
  FUseAttributes := False;
  FIncludeFolder := True;
end;

procedure TMLRFileIterator.DoBeforeIterating(FileNames: TStrings);
begin
  if Assigned(FBeforeIterating) then
    FBeforeIterating(Self, FileNames);
end;

procedure TMLRFileIterator.DoFileFound(const FileName: string;
  var Continue: Boolean);
begin
  if Assigned(FFileFoundEvent) then
    FFileFoundEvent(Self, FileName, Continue);
end;

procedure TMLRFileIterator.Execute;
var
  Continue: Boolean;
  FileNames: TStrings;
  i: Integer;
begin
  FileNames := TStringList.Create;
  try
    ListFiles(FileNames);
    DoBeforeIterating(FileNames);
    Continue := True;
    i := 0;
    while (i < FileNames.Count) and Continue do begin
      DoFileFound(FileNames[i], Continue);
      Inc(i);
    end;
  finally
    FileNames.Free;
  end;
end;

function TMLRFileIterator.GetFullFilter: string;
begin
  Result := Slash(FFolder) + FFilter;
end;

procedure TMLRFileIterator.ListFiles(Strings: TStrings);
var
  Attr, Found: Integer;
  SearchRec: TSearchRec;
begin
  Strings.Clear;
  Strings.BeginUpdate;
  try
    if FUseAttributes then begin
      Attr := 0;
      if faReadOnly in FAttributes  then Attr := Attr + SysUtils.faReadOnly;
      if faHidden in FAttributes    then Attr := Attr + SysUtils.faHidden;
      if faSysFile in FAttributes   then Attr := Attr + SysUtils.faSysFile;
      if faVolumeID in FAttributes  then Attr := Attr + SysUtils.faVolumeID;
      if faDirectory in FAttributes then Attr := Attr + SysUtils.faDirectory;
      if faArchive in FAttributes   then Attr := Attr + SysUtils.faArchive;
    end else
      Attr := faAnyFile;
    Found := FindFirst(FullFilter, Attr, SearchRec);
    try
      while Found = 0 do begin
        if FIncludeFolder then
          Strings.Add(Slash(FFolder) + SearchRec.Name) else
          Strings.Add(SearchRec.Name);
        Found := FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TMLRFileIterator.SetFilter(const Value: string);
begin
  FFilter := Value;
end;

procedure TMLRFileIterator.SetFolder(const Value: string);
begin
  FFolder := Value;
end;

procedure TMLRFileIterator.SetFullFilter(const Value: string);
var
  i: Integer;
begin
  if Value = '' then begin
    FFilter := '';
    FFolder := '';
    exit;
  end;
  i := Length(Value);
  while (i > 0) and (Value[i] <> '\') do
    Dec(i);
  if Value[i] = '\' then begin
    FFilter := Copy(Value, i + 1, Length(Value));
    FFolder := Copy(Value, i, 1);
  end else begin
    FFilter := Value;
    FFolder := '';
  end;
end;

end.
