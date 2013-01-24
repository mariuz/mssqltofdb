{ IBCheckUpdatesUnit.pas
  This unit is responsible for automatic notification of updates. }

unit IBCheckUpdatesUnit;

interface

uses IBUpdateUnit;

type
  TCheckMode = (cmSilent, cmInteractive);

procedure CheckIBUpdates(const Mode: TCheckMode; const TopicName: string;
  LastUpdate: TDateTime);

implementation

uses
  Controls, Classes, Dialogs, Graphics, Forms, StdCtrls,
  SysUtils, WinInet, Windows;

type
  TFormFreer = class
    procedure OnClose(Sender: TObject; var Action: TCloseAction);
  end;

const
  //DefaultDocumentURL = 'http://www.xlprueba.com.ar/ib/ibupdates.txt';
  DefaultDocumentURL = 'http://www.ibphoenix.com/ibupdates.txt';
  //DefaultDocumentURL = 'http://localhost/ibupdates.txt';
  SHttpError = 'Update HTTP Error'#13#10'%s';

  SWinInetLibrary = 'wininet.dll';

function LoadLibraryFunctions(const LibraryName: string;
  FunctionNames: array of string;
  var FunctionEntryPoints: array of Pointer): THandle;
var   
  i: Integer;
begin
  Assert(High(FunctionNames) <= High(FunctionEntryPoints));

  Result := LoadLibrary(PChar(LibraryName));
  if Result = 0 then
    RaiseLastWin32Error;
  try
    for i := Low(FunctionNames) to High(FunctionNames) do begin
      FunctionEntryPoints[i] := GetProcAddress(Result,
        PChar(FunctionNames[i]));
      if FunctionEntryPoints[i] = nil then
        RaiseLastWin32Error;
    end;
  except
    FreeLibrary(Result);
    raise;
  end;
end;

{ This is a version of FetchHTML that will load the library dynamically. This
  change was inspired by H Guscott, who did not have the wininet.dll library. }
function DynamicFetchHTML(URL: string): string;
var
  DataBuffer: array[0..4095] of Char;
  dwCode: array[1..20] of char;
  dwIndex, dwCodeLen, dwRead, dwNumber: cardinal;
  hSession, hFile: hInternet;
  res: PChar;
  ResStr: string;
  Str: PChar;

  { Dynamic functions. }
  PInternetOpen: function (lpszAgent: PChar; dwAccessType: DWORD;
    lpszProxy, lpszProxyBypass: PChar; dwFlags: DWORD): HINTERNET; stdcall;
  PInternetOpenUrl: function (hInet: HINTERNET; lpszUrl: PChar;
    lpszHeaders: PChar; dwHeadersLength: DWORD; dwFlags: DWORD;
    dwContext: DWORD): HINTERNET; stdcall;
  PHttpQueryInfo: function (hRequest: HINTERNET; dwInfoLevel: DWORD;
    lpvBuffer: Pointer; var lpdwBufferLength: DWORD;
    var lpdwReserved: DWORD): BOOL; stdcall;
  PInternetReadFile: function (hFile: HINTERNET; lpBuffer: Pointer;
    dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;
  PInternetCloseHandle: function (hInet: HINTERNET): BOOL; stdcall;
  P: array[0..4] of Pointer;
  LibHandle: THandle;
begin
  LibHandle := LoadLibraryFunctions(SWinInetLibrary,
    ['InternetOpenA', 'InternetOpenUrlA', 'HttpQueryInfoA',
     'InternetReadFile', 'InternetCloseHandle'], P);
  try
    PInternetOpen := P[0];
    PInternetOpenUrl := P[1];
    PHttpQueryInfo := P[2];
    PInternetReadFile := P[3];
    PInternetCloseHandle := P[4];
    
    ResStr := '';
    if Pos('http://', LowerCase(URL)) = 0 then
      URL := 'http://' + URL;
    hSession := PInternetOpen('InetURL:/1.0',
      INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

    if Assigned(hSession) then begin
      hfile := PInternetOpenUrl(hSession, PChar(url), nil, 0,
        INTERNET_FLAG_RELOAD, 0);
      dwIndex  := 0;
      dwCodeLen := 10;
      PHttpQueryInfo(hFile, HTTP_QUERY_STATUS_CODE, @dwCode,
        dwCodeLen, dwIndex);
      res := PChar(@dwcode);
      dwNumber := sizeof(DataBuffer) - 1;
      if (res = '200') or (res = '302') then begin
        while (PInternetReadFile(hfile, @Databuffer, dwNumber, dwRead)) do begin
          if dwRead =0 then
            break;
          DataBuffer[dwRead] := #0;
          Str := PChar(@DataBuffer);
          ResStr := ResStr + Str;
        end;
      end else
        ResStr := 'Status:' + Res;
      if Assigned(hfile) then
        PInternetCloseHandle(hFile);
    end;
    PInternetCloseHandle(hSession);
    Result := ResStr;
  finally
    FreeLibrary(LibHandle);
  end;
end;

{ The following function was written by David Bolton, and was
  taken from the following URL:
  http://www.delphi3000.com/article.asp?ID=1326
  Permisson pending. Does anyone have his email? }
function FetchHTML(url: string):string;
var
  DataBuffer: array[0..4095] of Char;
  dwCode: array[1..20] of char;
  dwIndex, dwCodeLen, dwRead, dwNumber: cardinal;
  hSession, hFile: hInternet;
  res: PChar;
  ResStr: string;
  Str: PChar;
begin
  ResStr := '';
  if Pos('http://', LowerCase(url))=0 then
    url := 'http://' + url;
  hSession := InternetOpen('InetURL:/1.0',
    INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if Assigned(hsession) then begin
    hfile := InternetOpenUrl(hSession, PChar(url), nil, 0,
      INTERNET_FLAG_RELOAD, 0);
    dwIndex  := 0;
    dwCodeLen := 10;
    HttpQueryInfo(hfile, HTTP_QUERY_STATUS_CODE, @dwCode, dwCodeLen, dwIndex);
    res := PChar(@dwcode);
    dwNumber := sizeof(DataBuffer) - 1;
    if (res = '200') or (res = '302') then begin
      while (InternetReadFile(hfile, @Databuffer, dwNumber, dwRead)) do begin
        if dwRead =0 then
          break;
        DataBuffer[dwRead] := #0;
        Str := PChar(@DataBuffer);
        ResStr := ResStr + Str;
      end;
    end else
      ResStr := 'Status:' + Res;
    if Assigned(hfile) then
      InternetCloseHandle(hFile);
  end;
  InternetCloseHandle(hSession);
  Result := ResStr;
end;

procedure LoadDocumentFromURL(Document: TIBUpdateDocument;
  const URL: string);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    //Strings.Text := FetchHTML(URL);
    Strings.Text := DynamicFetchHTML(URL);
    if Copy(Strings[0], 1, 6) = 'Status' then
      raise Exception.CreateFmt(SHttpError, [Strings[0]]);
    Document.LoadFromStrings(Strings);
  finally
    Strings.Free;
  end;
end;

procedure ShowTopic(Topic: TIBUpdateTopic; Modal: Boolean);
var
  Form: TForm;
  Memo: TMemo;
begin
  Form := TForm.Create(nil);
  try
    Memo := TMemo.Create(Form);
    Memo.Parent := Form;

    Form.Caption := 'Update';
    Form.Width := (Screen.Width div 3) * 2;
    Form.Height := (Screen.Height div 3) * 2;
    Form.BorderStyle := bsDialog;

    Memo.Align := alClient;
    Memo.Font.Name := 'Courier';
    Memo.Color := clBtnFace;
    Memo.ReadOnly := True;
    Memo.ScrollBars := ssVertical;
    Memo.Lines.Add(Format('Update on %s [%s]',
      [Topic.Description, Topic.Name]));
    Memo.Lines.Add('');
    Memo.Lines.Add('Last update: ' + DateToStr(Topic.LastUpdate));
    Memo.Lines.Add('');
    Memo.Lines.AddStrings(Topic.Body);
    if Modal then begin
      Form.Position := poScreenCenter;
      Form.ShowModal;
    end else begin
      Form.Top := 0;
      Form.Left := 0;
      Form.OnClose := TFormFreer.Create.OnClose;
      { 2000.09.19 when running in its own thread, the
        form should be shown as modal, otherwise it flickers
        and disappears. Bug? Feature? }
      Form.ShowModal;
    end;
  except
    Form.Free;
    raise;
  end;
end;

function InternetImmediateAvailable: Boolean;
const
  INTERNET_CONNECTION_OFFLINE = $20;
var
  LibHandle: THandle;
  Flags: Cardinal;
  P: array[0..0] of Pointer;
  PInternetGetConnectedState: function (lpdwFlags: LPDWORD; dwReserved: DWORD): BOOL; stdcall;
begin
  LibHandle := LoadLibraryFunctions(SWinInetLibrary,
    ['InternetGetConnectedState'], P);
  try
    PInternetGetConnectedState := P[0];
    if PInternetGetConnectedState(@Flags, 0) = False then
      Result := False
    else
      Result := (Flags and INTERNET_CONNECTION_OFFLINE) = 0;
  finally
    FreeLibrary(LibHandle);
  end;
end;

function CanConnect: Boolean;
var
  LibHandle: THandle;
  P: array[0..0] of Pointer;
  PInternetAttemptConnect: function (dwReserved: DWORD): DWORD; stdcall;
begin
  LibHandle := LoadLibraryFunctions(SWinInetLibrary,
    ['InternetAttemptConnect'], P);
  try
    PInternetAttemptConnect := P[0];
    Result := PInternetAttemptConnect(0) = ERROR_SUCCESS;
  finally
    FreeLibrary(LibHandle);
  end;
end;

function GetUpdateURL: string;
var
  F: TextFile;
begin
  Result := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0))) +
    'check_here.txt';
  if FileExists(Result) then begin
    AssignFile(F, Result);
    Reset(F);
    try
      if Eof(F) then
        Result := ''else
        Readln(F, Result);
    finally
      CloseFile(F);
    end;
  end else
    Result := '';
  if Result = '' then
    Result := DefaultDocumentURL;
  Result := Trim(Result);
end;

procedure CheckIBUpdates(const Mode: TCheckMode;
  const TopicName: string; LastUpdate: TDateTime);
var
  Document: TIBUpdateDocument;
  Topic: TIBUpdateTopic;
  OldCursor: TCursor;
begin
  if Mode = cmSilent then begin
    try
      if not InternetImmediateAvailable then
        Exit;
    except
      { Ignore exceptions. }
    end;
  end;

  OldCursor := Screen.Cursor;
  if Mode = cmInteractive then begin
    if not CanConnect then
      Exit;
    Screen.Cursor := crHourglass;
  end;
  
  try
    Document := TIBUpdateDocument.Create;
    try
      LoadDocumentFromURL(Document, GetUpdateURL);
      Topic := Document.TopicByName(TopicName);
      if Assigned(Topic) and (Topic.LastUpdate > LastUpdate) then
        ShowTopic(Topic, Mode = cmInteractive)
      else if Mode = cmInteractive then
        ShowMessage(Format('No updates for %s.', [TopicName]));
    finally
      Screen.Cursor := OldCursor;
      Document.Free;
    end;
  except
    on E: Exception do begin
      if Mode = cmInteractive then
        raise;
    end;
  end;
end;

procedure TFormFreer.OnClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  Free;
end;

end.

