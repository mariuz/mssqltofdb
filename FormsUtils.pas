unit FormsUtils;

interface

uses
  Controls, Forms;

function AppFileName(const FileName: string): string;
procedure CenterForm(Form: TForm);
function CreateControlDialog(var Control; ControlClass: TControlClass): TForm;

implementation

uses
  Buttons, SysUtils;

function AppFileName(const FileName: string): string;
begin
  Result := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName)) +
    FileName;
end;

procedure CenterForm(Form: TForm);
begin
  Form.Left := (Screen.Width - Form.Width) div 2;
  Form.Top := (Screen.Height - Form.Height) div 2;
end;

function CreateControlDialog(var Control; ControlClass: TControlClass): TForm;
var
  AControl: TControl;
  Button: TBitBtn;
begin
  Result := TForm.Create(nil);
  try
    Result.Position := poScreenCenter;
    Result.BorderStyle := bsDialog;

    TControl(Control) := ControlClass.Create(Result);
    AControl := TControl(Control);
    AControl.Parent := Result;
    AControl.Left := 8;
    AControl.Top := 8;
    AControl.Width := Result.ClientWidth - 16;

    Button := TBitBtn.Create(Result);
    Button.Parent := Result;
    Button.Top := Result.ClientHeight - 8 - Button.Height;
    Button.Left := Result.ClientWidth - 8 - Button.Width;
    Button.Kind := bkCancel;

    Button := TBitBtn.Create(Result);
    Button.Parent := Result;
    Button.Top := Result.ClientHeight - 8 - Button.Height;
    Button.Left := Result.ClientWidth - 16 - Button.Width * 2;
    Button.Kind := bkOk;

    AControl.Height := Button.Top - 8 - AControl.Top;
  except
    Result.Free;
    raise;
  end;
end;

end.
