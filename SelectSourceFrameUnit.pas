
{*******************************************************}
{                                                       }
{    Select Source Frame                                }
{                                                       }
{    Provides controls to select an ADO source for      }
{    the migration process.                             }
{                                                       }
{*******************************************************}

unit SelectSourceFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DragDropUnit;

type
  TSelectSourceFrame = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    btnBuild: TButton;
    txtConnectionString: TMemo;
    btnSaveAsDefault: TButton;
    Label3: TLabel;
    procedure btnBuildClick(Sender: TObject);
    procedure btnSaveAsDefaultClick(Sender: TObject);
  private
    FDropMan: TDropTargetManager;
    function GetConnectionString: string;
    procedure FilesDropped(Sender: TObject; Files: TStrings);
    procedure SetConnectionString(const Value: string);
  public
    procedure EnableDragDrop;
    property ConnectionString: string read GetConnectionString write SetConnectionString;
  end;

implementation

uses ActiveX, ComObj, OLEDB, WizFlowUnit;

const
  SAccessExtension = '.MDB';
  SAccessSystemExtension = '.MDW';
  STABExtension = '.TAB';
  STSVExtension = '.TSV';
  SCSVExtension = '.CSV';
  STextExtension = '.TXT';

{$R *.DFM}

function EditConnectionString(var ConnectionString: string): Boolean;
var
  DataInitialize: IDataInitialize;
  DataSource: IDBProperties;
  InitString: PWideChar;
  Prompt: IDBPromptInitialize;
  Res: HResult;
  WideConnection: WideString;
begin
  OleCheck(CoCreateInstance(CLSID_DATALINKS, nil, CLSCTX_INPROC_SERVER,
    IDBPromptInitialize, Prompt));
  DataInitialize := Prompt as IDataInitialize;
  { If ConnectionString is not empty, we can create a data source object
    which is used as a starting point. }
  if ConnectionString <> '' then begin
    WideConnection := ConnectionString;
    OleCheck(DataInitialize.GetDataSource(nil, CLSCTX_INPROC_SERVER,
      PWideChar(WideConnection), IDBProperties, IUnknown(DataSource)));
  end;
  Res := Prompt.PromptDataSource(nil, Application.Handle,
    DBPROMPTOPTIONS_PROPERTYSHEET, 0, nil, nil, IUnknown,
    IUnknown(DataSource));
  if Res = S_OK then begin
    OleCheck(DataInitialize.GetInitializationString(DataSource, True,
      InitString));
    WideConnection := InitString;
    ConnectionString := WideConnection;
    SysFreeString(InitString);
    Result := True;
  end else
    Result := False;
end;

procedure TSelectSourceFrame.btnBuildClick(Sender: TObject);
var
  ConnectionString: string;
begin
  ConnectionString := txtConnectionString.Text;
  if EditConnectionString(ConnectionString) then
    txtConnectionString.Text := ConnectionString;
end;

procedure TSelectSourceFrame.EnableDragDrop;
begin
  if not Assigned(FDropMan) then
    FDropMan := TDropTargetManager.Create(Self);
  FDropMan.Target := txtConnectionString;
  FDropMan.AcceptFiles := True;
  FDropMan.AcceptText := False;
  FDropMan.Active := True;
  FDropMan.OnFilesDropped := FilesDropped;
end;

procedure TSelectSourceFrame.FilesDropped(Sender: TObject;
  Files: TStrings);
var
  FileExt: string;
  FileName: string;
  Strings: TStrings;
begin
  if Files.Count = 0 then
    Exit;
  FileName := Files[0];
  Strings := txtConnectionString.Lines;
  FileExt := UpperCase(ExtractFileExt(FileName));
  if FileExt = SAccessExtension then begin
    Strings.Clear;
    Strings.Add('Provider=Microsoft.Jet.OLEDB.4.0;');
    Strings.Add('Password="";');
    Strings.Add('Data Source=' + FileName + ';');
    Strings.Add('Persist Security Info=True;');
  end else if FileExt = SAccessSystemExtension then begin
    Strings.Add('Jet OLEDB:System database=' +
      FileName + ';');
    if AnsiPos('USER ID', UpperCase(Strings.Text)) = -1 then
      Strings.Add('User ID=enter your username');
    if AnsiPos('PASSWORD', UpperCase(Strings.Text)) = -1 then
      Strings.Add('Password=enter your username');
  end else if (FileExt = STextExtension) or
              (FileExt = STABExtension) or
              (FileExt = STSVExtension) or
              (FileExt = SCSVExtension) then begin
    Strings.Clear;
    Strings.Add('Provider=MSDASQL.1;');
    Strings.Add('Driver={Microsoft Text Driver (*.txt; *.csv)};');
    Strings.Add('Extensions List=' + LowerCase(FileExt));
    Strings.Add('DBQ=' + ExtractFileDir(FileName) + ';');
    Strings.Add('DefaultDir=' + ExtractFileDir(FileName) + ';');
    Strings.Add('FIL=text;');
  end;
end;

function TSelectSourceFrame.GetConnectionString: string;
begin
  Result := txtConnectionString.Text;
end;

procedure TSelectSourceFrame.btnSaveAsDefaultClick(Sender: TObject);
begin
  Flow.SaveDefaultSource(txtConnectionString.Lines);
end;

procedure TSelectSourceFrame.SetConnectionString(const Value: string);
begin
  txtConnectionString.Text := Value;
end;

end.
