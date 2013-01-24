{*******************************************************}
{                                                       }
{    SQL2GDB Command Line Library                       }
{                                                       }
{    Provides a routine to control the wizard from      }
{    the command line.                                  }
{                                                       }
{*******************************************************}

unit SQL2GDBCmdLineUnit;

interface

function RunFromCommandLine: Boolean;

implementation

uses
  ConvertInfoUnit, SysUtils;

function GetScriptFileName: string;
begin
  Result := ParamStr(1);
end;

procedure LogError(const Msg: string);
var
  F: TextFile;
  FileName: string;
begin
  FileName := ChangeFileExt(ParamStr(0), '_err.txt');
  AssignFile(F, FileName);
  if FileExists(FileName) then
    Append(F)
  else
    Rewrite(F);
  try
    Writeln(F, '[' + FormatDateTime('yyyy.mm.dd hh:nn:ss', Now) + '] ' + Msg);
  finally
    CloseFile(F);
  end;
end;

procedure LogErrorFmt(const Fmt: string; const Args: array of const);
begin
  LogError(Format(Fmt, Args));
end;

function RunFromCommandLine: Boolean;
var
  Convert: TConvertInfo;
  ScriptFile: string;
begin
  if ParamCount = 0 then begin
    Result := False;
    Exit;
  end;
  Result := True;
  ScriptFile := GetScriptFileName;
  Convert := TConvertInfo.Create;
  try
    Convert.LoadFromScript(ScriptFile);
    Convert.Execute;
    Convert.Free;
  except
    on E: Exception do
      LogErrorFmt('Exception %s: %s', [E.ClassName, E.Message]);
  end;
end;

end.
