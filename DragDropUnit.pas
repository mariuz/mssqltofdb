
{*******************************************************}
{                                                       }
{    Drag and Drop Library                              }
{                                                       }
{    Provides classes and routines to work with         }
{    COM drag and drop mechanism.                       }
{                                                       }
{*******************************************************}

unit DragDropUnit;

interface

uses
  Windows, SysUtils, ActiveX, Controls, Classes;

type
  TAcceptEvent = procedure (Sender :TObject; DataObject :IDataObject;
    var Accept :Boolean) of object;
  TDragEnterEvent = procedure (Sender :TObject; const dataObj: IDataObject;
    grfKeyState: Integer; pt: TPoint; var dwEffect: Integer; var Res :HResult) of object;
  TDragLeaveEvent = procedure (Sender :TObject; var Res :HResult) of object;
  TDropEvent = procedure (Sender :TObject; const dataObj: IDataObject; grfKeyState: Integer;
      pt: TPoint; var dwEffect: Integer; var Res :HResult) of object;
  TDragOverEvent = procedure (Sender :TObject; grfKeyState: Integer; pt: TPoint;
    var dwEffect: Integer; var Res :HResult) of object;
  TFilesDroppedEvent = procedure (Sender: TObject; Files: TStrings) of object;
  TTextDroppedEvent = procedure (Sender :TObject; Text :string) of object;

  TMyDropTarget = class;

  TDropTargetManager = class(TComponent)
  private
    FAcceptText: Boolean;
    FActive: Boolean;
    FAcceptEvent: TAcceptEvent;
    FAcceptFiles: Boolean;
    FOnFilesDropped: TFilesDroppedEvent;
    FLastEffect :Cardinal;
    FWinControl: TWinControl;
    FOnDragEnter: TDragEnterEvent;
    FOnDragLeave: TDragLeaveEvent;
    FOnDragOver: TDragOverEvent;
    FOnDragDrop: TDropEvent;
    FOnTextDropped: TTextDroppedEvent;
    FIsDragging     :Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetWinControl(const Value: TWinControl);
  protected
    FMyDropTarget :TMyDropTarget;
    procedure Notification(AComponent :TComponent; Operation :TOperation); override;
    procedure Loaded; override;
    { Functions called from MyDropTarget. }
    function DragEnter(const dataObj: IDataObject; grfKeyState: Integer;
      pt: TPoint; var dwEffect: Integer): HResult;
    function DragLeave: HResult;
    function Drop(const dataObj: IDataObject; grfKeyState: Integer;
      pt: TPoint; var dwEffect: Integer): HResult;
    function DragOver(grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
  public
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
    property Dragging: Boolean read FIsDragging;
  published
    property Active: Boolean read FActive write SetActive default True;
    property AcceptFiles: Boolean read FAcceptFiles write FAcceptFiles default False;
    property AcceptText: Boolean read FAcceptText write FAcceptText default True;
    property Target: TWinControl read FWinControl write SetWinControl;
    property OnAccept: TAcceptEvent read FAcceptEvent write FAcceptEvent;
    property OnDragEnter: TDragEnterEvent read FOnDragEnter write FOnDragEnter;
    property OnDragLeave: TDragLeaveEvent read FOnDragLeave write FOnDragLeave;
    property OnDragDrop: TDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnFilesDropped: TFilesDroppedEvent read FOnFilesDropped write FOnFilesDropped;
    property OnTextDropped: TTextDroppedEvent read FOnTextDropped write FOnTextDropped;
  end;

  TMyDropTarget = class(TInterfacedObject, IDropTarget)
  private
    FDropTargetMan: TDropTargetManager;
    procedure SetWinControl(const Value: TWinControl);
  protected
    FWinControl :TWinControl;
    FLastEffect :Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    property WinControl :TWinControl read FWinControl write SetWinControl;
    property DropTargetManager  :TDropTargetManager read FDropTargetMan write FDropTargetMan;
  end;

procedure ListFiles(DataObject: IDataObject; Strings: TStrings;
  ShortPath: Boolean = False);
function GlobalMemToString(Handle: HGlobal): string;
procedure Register;

implementation

uses
  Dialogs, ShellApi;

const
  DVASPECT_SHORTNAME  = 2; // use for CF_HDROP to get short name version of file paths
  DVASPECT_COPY       = 3; // use to indicate format is a "Copy" of the data (FILECONTENTS, FILEDESCRIPTOR, etc)
  DVASPECT_LINK       = 4; // use to indicate format is a "Shortcut" to the data (FILECONTENTS, FILEDESCRIPTOR, etc)

procedure CheckQueryGetData(Res: HResult);
var
  Err: string;
begin
  Err := '';
  case Res of
    E_INVALIDARG:
      Err := 'One or more arguments are invalid.';
    E_UNEXPECTED:
      Err := 'An unexpected error occurred.';
    E_OUTOFMEMORY:
      Err := 'The process didn''t execute because the system ran out of memory.';
    DV_E_LINDEX:
      Err := 'Invalid value for lindex; currently, only -1 is supported.';
    DV_E_FORMATETC:
      Err := 'Invalid value for pFormatetc.';
    DV_E_TYMED:
      Err := 'Invalid tymed value.';
    DV_E_DVASPECT:
      Err := 'Invalid dwAspect value.';
    OLE_E_NOTRUNNING:
      Err := 'Object application is not running.';
    else
      exit;
  end;
  if Err <> '' then
    raise Exception.Create(Err);
end;

function DragEnterText(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  FormatEtc :TFormatEtc;
  Res       :HResult;
begin
  with FormatEtc do begin
    FillChar(FormatEtc, sizeof(FormatEtc), #0);
    cfFormat  := CF_TEXT;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  Res := dataObj.QueryGetData(FormatEtc);
  if Res = S_OK then
    dwEffect := DROPEFFECT_COPY
  else
    dwEffect := DROPEFFECT_NONE;
  Result := S_OK;
end;

function DragEnterFiles(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  FormatEtc: TFormatEtc;
  Res: HResult;
begin
  with FormatEtc do begin
    FillChar(FormatEtc, sizeof(FormatEtc), #0);
    cfFormat := CF_HDROP;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  Res := dataObj.QueryGetData(FormatEtc);
  if Res = S_OK then
    dwEffect := DROPEFFECT_COPY
  else
    dwEffect := DROPEFFECT_NONE;
  Result := S_OK;
end;

function DropText(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer;
  var Data :string): HResult;
var
  FormatEtc :TFormatEtc;
  Medium: TStgMedium;
  Res: HResult;
begin
  with FormatEtc do begin
    FillChar(FormatEtc, sizeof(FormatEtc), #0);
    cfFormat := CF_TEXT;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  with Medium do begin
    FillChar(Medium, sizeof(Medium), #0);
    tymed := TYMED_HGLOBAL;
  end;
  Res := dataObj.GetData(FormatEtc, Medium);
  if Res = S_OK then begin
    dwEffect := DROPEFFECT_COPY;
    Data := GlobalMemToString(Medium.hGlobal);
    ReleaseStgMedium(Medium);
  end else
    dwEffect := DROPEFFECT_NONE;
  Result := S_OK;
end;

function DropFiles(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer;
  Strings: TStrings): HResult;
begin
  ListFiles(dataObj, Strings, False);
  Result := S_OK;
end;

procedure ListFiles(DataObject: IDataObject; Strings: TStrings;
  ShortPath: Boolean);
var
  FileCount: Integer;
  FileName: array[0..MAX_PATH-1] of char;
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  Data: HDrop;
begin
  with FormatEtc do begin
    cfFormat := CF_HDROP;
    ptd := nil;
    if ShortPath then
      dwAspect := DVASPECT_SHORTNAME
    else
      dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  CheckQueryGetData(DataObject.GetData(FormatEtc, Medium));
  Data := Medium.hGlobal;
  FileCount := DragQueryFile(Data, $FFFFFFFF, nil, 0);
  while FileCount > 0 do begin
    DragQueryFile(Data, FileCount - 1, FileName, sizeof(FileName));
    Strings.Add(string(FileName));
    Dec(FileCount);
  end;
  ReleaseStgMedium(Medium);
end;

function GlobalMemToString(Handle: HGlobal): string;
var
  MemData: PChar;
  DataSize: Integer;
begin
  DataSize := GlobalSize(Handle);
  if DataSize = 0 then
    Result := ''
  else begin
    SetLength(Result, DataSize);
    MemData := PChar(GlobalLock(Handle));
    try
      if Assigned(MemData) then
        Move(MemData^, PChar(Result)^, DataSize)
      else
        Result := '';
    finally
      GlobalUnlock(Handle);
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TDropTargetManager]);
end;

{ TMyDropTarget }

constructor TMyDropTarget.Create;
begin
  _AddRef;
end;

destructor TMyDropTarget.Destroy;
begin
  inherited Destroy;
end;

function TMyDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin
  if Assigned(FDropTargetMan) then begin
    Result := FDropTargetMan.DragEnter(dataObj, grfKeyState, pt, dwEffect);
    exit;
  end;
  dwEffect  := DROPEFFECT_NONE;
  Result    := S_OK;
end;

function TMyDropTarget.DragLeave: HResult;
begin
  if Assigned(FDropTargetMan) then begin
    Result := FDropTargetMan.DragLeave;
    exit;
  end;
  Result := S_OK;
end;

function TMyDropTarget.DragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
begin
  if Assigned(FDropTargetMan) then begin
    Result := FDropTargetMan.DragOver(grfKeyState, pt, dwEffect);
    exit;
  end;
  dwEffect := FLastEffect;
  Result := S_OK;
end;

function TMyDropTarget.Drop(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin
  if Assigned(FDropTargetMan) then begin
    Result := FDropTargetMan.Drop(dataObj, grfKeyState, pt, dwEffect);
    exit;
  end;
  dwEffect := DROPEFFECT_NONE;
  Result := S_OK;
end;

procedure TMyDropTarget.SetWinControl(const Value: TWinControl);
begin
  FWinControl := Value;
  if Assigned(Value) then
    RegisterDragDrop(Value.HAndle, Self as IDropTarget);
end;

{ TDropTargetManager }

constructor TDropTargetManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
  FAcceptText := True;
  FIsDragging := False;
  if not (csDesigning in ComponentState) then
    OleInitialize(nil);
end;

destructor TDropTargetManager.Destroy;
begin
  if not (csDesigning in ComponentState) then
    OleUninitialize;
  Active := False;
  inherited Destroy;
end;

function TDropTargetManager.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin
  FIsDragging := True;
  dwEffect  := DROPEFFECT_NONE;
  Result    := S_OK;
  if FAcceptText then
    Result := DragEnterText(dataObj, grfKeyState, pt, dwEffect);
  if FAcceptFiles then
    Result := DragEnterFiles(dataObj, grfKeyState, pt, dwEffect);
  if Assigned(FOnDragEnter) then
    FOnDragEnter(Self, dataObj, grfKeyState, pt, dwEffect, Result);
  FLastEffect := dwEffect;
end;

function TDropTargetManager.DragLeave: HResult;
begin
  Result := S_OK;
  if Assigned(FOnDragLeave) then
    FOnDragLeave(Self, Result);
  FIsDragging := False;
end;

function TDropTargetManager.DragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
begin
  FIsDragging := True;
  dwEffect := FLastEffect;
  Result := S_OK;
  if Assigned(FOnDragOver) then
    FOnDragOver(Self, grfKeyState, pt, dwEffect, Result);
  FLastEffect := dwEffect;
end;

function TDropTargetManager.Drop(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  Data: string;
  Strings: TStringList;
begin
  dwEffect := FLastEffect;
  Result   := S_OK;
  if FAcceptText then
    Result := DropText(dataObj, grfKeyState, pt, dwEffect, Data);
  if Assigned(FOnDragDrop) then
    FOnDragDrop(Self, dataObj, grfKeyState, pt, dwEffect, Result);
  if FAcceptText and (dwEffect <> DROPEFFECT_NONE) and Assigned(FOnTextDropped) then
    FOnTextDropped(Self, Data)
  else if FAcceptFiles and Assigned(FOnFilesDropped) then begin
    Strings := TStringList.Create;
    try
      Strings.Sorted := True;
      Result := DropFiles(dataObj, grfKeyState, pt, dwEffect, Strings);
      if dwEffect <> DROPEFFECT_NONE then
        FOnFilesDropped(Self, Strings);
    finally
      Strings.Free;
    end;
  end;
  FIsDragging := False;
end;

procedure TDropTargetManager.Loaded;
begin
  inherited Loaded;
  if FActive then begin
    FActive := False;
    Active := True;
  end;
end;

procedure TDropTargetManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FWinControl) then begin
    Active := False;
    FWinControl := nil;
  end;
end;

procedure TDropTargetManager.SetActive(const Value: Boolean);
var
  DropTarget: TMyDropTarget;
begin
  if csLoading in ComponentState then begin
    FActive := Value;
    exit;
  end;
  if Value = FActive then exit;
  if not (csDesigning in ComponentState) then begin
    if Value then begin
      if not Assigned(FWinControl) then exit;
      DropTarget := TMyDropTarget.Create;
      DropTarget.WinControl := FWinControl;
      DropTarget.DropTargetManager := Self;
      (DropTarget as IUnknown)._Release;
    end else begin
      if Assigned(FWinControl) and FWinControl.HandleAllocated then
        RevokeDragDrop(FWinControl.Handle);
    end;
  end;
  FActive := Value;
end;

procedure TDropTargetManager.SetWinControl(const Value: TWinControl);
var
  OldActive: Boolean;
begin
  OldActive := False;
  if Active then begin
    OldActive := True;
    Active := False;
  end;
  FWinControl := Value;
  if Assigned(FWinControl) then begin
    FWinControl.FreeNotification(Self);
    if OldActive then
      Active := True;
  end;
end;

end.


