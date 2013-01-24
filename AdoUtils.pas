
{*******************************************************}
{                                                       }
{     ADO Utility Library                               }
{                                                       }
{     This is an adapter version for the sql2gdb app.   }
{                                                       }
{*******************************************************}

unit AdoUtils;

interface

uses
  ADODB_TLB, Classes;

function ADOOpenConnect(const SourceConnectionString: string): _Connection;
function ConnectionSelectTables(Conn: _Connection;
  TableNames: TStrings; const Prompt: string = ''): Boolean;
procedure GetTableNames(Conn: _Connection; TableNames: TStrings);

implementation

uses
  CheckLst, CheckLstUtils, Controls, Forms, FormsUtils;

function ADOOpenConnect(const SourceConnectionString: string): _Connection;
begin
  Result := CoConnection.Create;
  Result.Open(SourceConnectionString, '', '', 0);
end;

function ConnectionSelectTables(Conn: _Connection;
  TableNames: TStrings; const Prompt: string = ''): Boolean;
var
  CheckListBox: TCheckListBox;
  Form: TForm;
begin
  Form := CreateControlDialog(CheckListBox, TCheckListBox);
  try
    Form.Caption := Prompt;
    GetTableNames(Conn, CheckListBox.Items);
    CheckItems(CheckListBox, TableNames);
    Result := Form.ShowModal = mrOk;
    if Result then begin
      TableNames.Clear;
      ListCheckedItems(CheckListBox, TableNames);
    end;
  finally
    Form.Free;
  end;
end;

procedure GetTableNames(Conn: _Connection; TableNames: TStrings);
var
  TableName: Field;
  TableType: Field;
  Rst: Recordset;
begin
  TableNames.Clear;
  Rst := Conn.OpenSchema(adSchemaTables, EmptyParam, EmptyParam);
  TableName := Rst.Fields.Get_Item('TABLE_NAME');
  TableType := Rst.Fields.Get_Item('TABLE_TYPE');
  while not Rst.Eof do begin
    if TableType.Value = 'TABLE' then
      TableNames.Add(TableName.Value);
    Rst.MoveNext;
  end;
  Rst.Close;
end;

end.
