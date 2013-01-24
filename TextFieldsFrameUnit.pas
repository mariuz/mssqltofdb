{*****************************************************************************}
{                                                                             }
{ Text Fields Unit                                                            }
{ Displays the schema.ini file for editing                                    }
{                                                                             }
{*****************************************************************************}

unit TextFieldsFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Grids, TextModelUnit;

type
  TTextFieldsFrame = class(TFrame, ITextInfoSupport)
    grdFieldDefs: TStringGrid;
    btnSave: TButton;
    Label1: TLabel;
    lblFieldTypes: TLabel;
    procedure btnSaveClick(Sender: TObject);
  private
    FTextInfo: TTextInfo;
    FOnSave: TNotifyEvent;
  protected
    { ITextInfoSupport implementation. }
    function GetTextInfo: TTextInfo;
    procedure SetTextInfo(Value: TTextInfo);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadValuesToGrid;
    procedure SaveValuesFromGrid;
    property TextInfo: TTextInfo read FTextInfo write FTextInfo;
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
  end;

implementation

{$R *.DFM}

procedure TTextFieldsFrame.LoadValuesToGrid;
var
  ColInfo: TColInfo;
  i: Integer;
  Row: Integer;
begin
  for i := 0 to TextInfo.SchemaColumns.Count - 1 do begin
    Row := i + 1;
    grdFieldDefs.Cells[0, Row] := '#' + IntToStr(i);
    ColInfo := StrToColInfo(TextInfo.SchemaColumns[i]);
    if ColInfo.Name <> '' then
      grdFieldDefs.Cells[1, Row] := ColInfo.Name;
    if ColInfo.FieldType <> '' then
      grdFieldDefs.Cells[2, Row] := ColInfo.FieldType;
    if ColInfo.Width <> 0 then
      grdFieldDefs.Cells[3, Row] := IntToStr(ColInfo.Width);
    { TODO: handle extras, such as date formats. }
  end;
end;

constructor TTextFieldsFrame.Create(AOwner: TComponent);
begin
  inherited;
  grdFieldDefs.Cells[0, 0] := 'Field';
  grdFieldDefs.Cells[1, 0] := 'Field Name';
  grdFieldDefs.Cells[2, 0] := 'Data Type';
  grdFieldDefs.Cells[3, 0] := 'Size';

  btnSave.Left := ClientWidth - btnSave.Width - 8;
  btnSave.Top := ClientHeight - btnSave.Height - 8;

  grdFieldDefs.Height := btnSave.Top - 8 - grdFieldDefs.Top; 
end;

procedure TTextFieldsFrame.btnSaveClick(Sender: TObject);
begin
  SaveValuesFromGrid;
  TextInfo.SaveSchemaToFile;
  if Assigned(FOnSave) then
    FOnSave(Self);
end;

{ put editied grid values back to SchemaColumns string list }
procedure TTextFieldsFrame.SaveValuesFromGrid;
var
  ACol: string;
  AName: string;
  AType: string;
  ASize: string;
  i: Integer;
  Def: string;
begin
  TextInfo.SchemaColumns.Clear;
  for i := 1 to grdFieldDefs.RowCount - 1 do begin
    ACol := Trim(grdFieldDefs.Cells[0, i]);
    AName := Trim(grdFieldDefs.Cells[1, i]);
    AType := Trim(grdFieldDefs.Cells[2, i]);
    ASize := Trim(grdFieldDefs.Cells[3, i]);
    if AName = '' then continue;
    Def := Format('Col%d=%s %s', [i, AName, AType]);
    if ASize <> '' then
      Def := Def + Format(' Width %s', [ASize]);
    TextInfo.SchemaColumns.Add(Def);
  end;
end;

function TTextFieldsFrame.GetTextInfo: TTextInfo;
begin
  Result := FTextInfo;
end;

procedure TTextFieldsFrame.SetTextInfo(Value: TTextInfo);
begin
  FTextInfo := Value;
end;

end.
