unit CheckLstUtils;

interface

uses
  CheckLst, Classes;

procedure CheckItems(CheckList: TCheckListBox; Items: TStrings);
procedure ListCheckedItems(CheckList: TCheckListBox; Items: TStrings);

implementation

procedure CheckItems(CheckList: TCheckListBox; Items: TStrings);
var
  i: Integer;
  Index: Integer;
begin
  for i := 0 to Items.Count - 1 do begin
    Index := CheckList.Items.IndexOf(Items[i]);
    if Index <> -1 then
      CheckList.Checked[Index] := True;
  end;
end;

procedure ListCheckedItems(CheckList: TCheckListBox; Items: TStrings);
var
  i: Integer;
begin
  for i := 0 to CheckList.Items.Count - 1 do
    if CheckList.Checked[i] then
      Items.Add(CheckList.Items[i]);
end;

end.
