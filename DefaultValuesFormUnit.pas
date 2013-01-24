
{*******************************************************}
{                                                       }
{    SQL2GDB Application                                }
{                                                       }
{    Provides a form to edit the migration default      }
{    field values.                                      }
{                                                       }
{*******************************************************}

unit DefaultValuesFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ExtCtrls, StdCtrls, Buttons, DragDropUnit;

type
  TDefaultValuesForm = class(TForm)
    Memo1: TMemo;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Bevel1: TBevel;
    grdDefaultValues: TStringGrid;
    btnSaveToFile: TButton;
    btnLoadFromFile: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure grdDefaultValuesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnLoadFromFileClick(Sender: TObject);
  private
    FDropMan: TDropTargetManager;
    procedure FilesDropped(Sender: TObject; Files: TStrings);
  public
    procedure LoadValues(Strings: TStrings);
    procedure SaveValues(Strings: TStrings);
  end;

procedure EditDefaultValues(Values: TStrings);

implementation

{$R *.DFM}

procedure EditDefaultValues(Values: TStrings);
var
  DefaultValuesForm: TDefaultValuesForm;
begin
  DefaultValuesForm := TDefaultValuesForm.Create(Application);
  try
    DefaultValuesForm.LoadValues(Values);
    if DefaultValuesForm.ShowModal <> mrOk then Exit;
    Values.Clear;
    DefaultValuesForm.SaveValues(Values);
  finally
    DefaultValuesForm.Free;
  end;
end;

{ TDefaultValuesForm }

procedure TDefaultValuesForm.LoadValues(Strings: TStrings);
var
  EquPos: Integer;
  i: Integer;
  Line: string;
  RowIndex: Integer;
begin
  grdDefaultValues.RowCount := Strings.Count + 2;
  RowIndex := 1;
  for i := 0 to Strings.Count - 1 do begin
    Line := Strings[i];
    EquPos := AnsiPos('=', Line);
    if EquPos = 0 then continue;
    grdDefaultValues.Cells[0, RowIndex] := Copy(Line, 1, EquPos - 1);
    grdDefaultValues.Cells[1, RowIndex] := Copy(Line, EquPos + 1, Length(Line));
    Inc(RowIndex);
  end;
end;

procedure TDefaultValuesForm.SaveValues(Strings: TStrings);
var
  AName, AValue: string;
  i: Integer;
begin
  for i := 1 to grdDefaultValues.RowCount - 1 do begin
    AName := Trim(grdDefaultValues.Cells[0, i]);
    AValue := grdDefaultValues.Cells[1, i];
    if AName = '' then continue;
    Strings.Append(AName + '=' + AValue);
  end;
end;

procedure TDefaultValuesForm.FormCreate(Sender: TObject);
begin
  if not Assigned(FDropMan) then begin
    FDropMan := TDropTargetManager.Create(Self);
    FDropMan.Target := grdDefaultValues;
    FDropMan.AcceptFiles := True;
    FDropMan.AcceptText := False;
    FDropMan.Active := True;
    FDropMan.OnFilesDropped := FilesDropped;
  end;
  grdDefaultValues.Cells[0, 0] := 'Table/Field Name';
  grdDefaultValues.Cells[1, 0] := 'Constant Value';
end;

procedure TDefaultValuesForm.grdDefaultValuesKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) and
     (grdDefaultValues.Selection.Top = grdDefaultValues.RowCount-1) then
    grdDefaultValues.RowCount := grdDefaultValues.RowCount + 1;
end;

procedure TDefaultValuesForm.btnSaveToFileClick(Sender: TObject);
var
  Strings: TStringList;
begin
  if not dlgSave.Execute then Exit;
  Strings := TStringList.Create;
  try
    SaveValues(Strings);
    Strings.SaveToFile(dlgSave.FileName);
  finally
    Strings.Free;
  end;
end;

procedure TDefaultValuesForm.btnLoadFromFileClick(Sender: TObject);
var
  Strings: TStringList;
begin
  if not dlgOpen.Execute then Exit;
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(dlgOpen.FileName);
    LoadValues(Strings);
  finally
    Strings.Free;
  end;
end;

procedure TDefaultValuesForm.FilesDropped(Sender: TObject;
  Files: TStrings);
var
  FileName: string;
  Strings: TStringList;
begin
  if Files.Count = 0 then Exit;
  FileName := Files[0];
  if dlgOpen.FileName = '' then dlgOpen.FileName := FileName;
  if dlgSave.FileName = '' then dlgSave.FileName := FileName;
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(FileName);
    LoadValues(Strings);
  finally
    Strings.Free;
  end;
end;

end.
