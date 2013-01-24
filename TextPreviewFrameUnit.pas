{*****************************************************************************}
{                                                                             }
{ Text Preview Frame Unit                                                     }
{ Allows the user to browse or drag and drop a text file to work with.        }
{                                                                             }
{*****************************************************************************}

unit TextPreviewFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, DragDropUnit, TextModelUnit;

type
  TSMemo = class(TMemo)
    procedure WM_HScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WM_VScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  end;

  TTextPreviewFrame = class(TFrame, ITextInfoSupport)
    btnBrowse: TButton;
    dlgOpenText: TOpenDialog;
    pnlPreview: TPanel;
    Label1: TLabel;
    procedure btnBrowseClick(Sender: TObject);
  private
    mmoFilePreview: TSMemo;
    FTextInfo: TTextInfo;
    FDropMan: TDropTargetManager;
    procedure FilesDropped(Sender: TObject; Files: TStrings);
    procedure OpenFile(const FileName: string);
    procedure SetPreviewTabs;
  protected
    { ITextInfoSupport implementation. }
    function GetTextInfo: TTextInfo;
    procedure SetTextInfo(Value: TTextInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EnableDragDrop;
    property TextInfo: TTextInfo read FTextInfo write FTextInfo;
  end;


implementation

uses
  MLRTextColEditorUnit, TextFormUnit;

{$R *.DFM}

procedure TTextPreviewFrame.btnBrowseClick(Sender: TObject);
begin
  if dlgOpenText.Execute then
    OpenFile(dlgOpenText.FileName);
end;

procedure TTextPreviewFrame.EnableDragDrop;
begin
  if not Assigned(FDropMan) then
    FDropMan := TDropTargetManager.Create(Self);
  FDropMan.Target := mmoFilePreview;
  FDropMan.AcceptFiles := True;
  FDropMan.AcceptText := False;
  FDropMan.Active := True;
  FDropMan.OnFilesDropped := FilesDropped;
end;

procedure TTextPreviewFrame.FilesDropped(Sender: TObject;
  Files: TStrings);
begin
  if Files.Count = 0 then Exit;
  OpenFile(Files[0]);
end;

procedure TTextPreviewFrame.SetPreviewTabs;
const
  TabInc: LongInt = 50;
begin
  SendMessage(mmoFilePreview.Handle, EM_SETTABSTOPS, 1, LongInt(@TabInc));
end;

constructor TTextPreviewFrame.Create(AOwner: TComponent);
begin
  inherited;
  mmoFilePreview := TSMemo.Create(Self);
  with mmoFilePreview do begin
    Parent := Self;
    Width := 510;
    Height := 170;
    Left := 0;
    Font.Name := 'Courier New';
    Font.Size := 8;
    ReadOnly := True;
    ScrollBars := ssBoth;
    Top := 16;
    WantTabs := True;
    WordWrap := False;
    Anchors := [akLeft, akTop, akRight, akBottom];
  end;
  SetPreviewTabs;
end;

destructor TTextPreviewFrame.Destroy;
begin
  mmoFilePreview.Free;
  inherited;
end;

{ TSMemo }

procedure TSMemo.WM_HScroll(var Msg: TWMHScroll);
begin
  //ShowMessage('HScroll');
  inherited;
end;

procedure TSMemo.WM_VScroll(var Msg: TWMVScroll);
begin
  //ShowMessage('VScroll');
  inherited;
end;

function TTextPreviewFrame.GetTextInfo: TTextInfo;
begin
  Result := FTextInfo;
end;

procedure TTextPreviewFrame.SetTextInfo(Value: TTextInfo);
begin
  FTextInfo := Value;
end;

procedure TTextPreviewFrame.OpenFile(const FileName: string);
begin
  TextInfo.TextFileName := FileName;
  mmoFilePreview.Lines.Clear;
  ListTopLines(TextInfo.TextFileName, 255, mmoFilePreview.Lines);
  SetPreviewTabs;
end;

end.
