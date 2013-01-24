
{*******************************************************}
{                                                       }
{    Select InterBase Frame                             }
{                                                       }
{    Provides controls to select a destination          }
{    database and specify the connection behaviour.     }
{                                                       }
{*******************************************************}

unit SelectIBFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ConvertInfoUnit;

type
  TSelectIBFrame = class(TFrame)
    lblTitle: TLabel;
    lblSelectIB: TLabel;
    txtIB: TEdit;
    btnBrowseIB: TButton;
    dlgSelectIB: TSaveDialog;
    chkDeleteExisting: TCheckBox;
    lblIBUserName: TLabel;
    lblPassword: TLabel;
    txtIBUserName: TEdit;
    txtIBPassword: TEdit;
    Label3: TLabel;
    lblOutputFile: TLabel;
    chkOutputFile: TCheckBox;
    lblOutputFileName: TLabel;
    txtOutputFile: TEdit;
    btnBrowseOutputFile: TButton;
    dlgSaveOutputFile: TSaveDialog;
    chkUseExisting: TCheckBox;
    procedure btnBrowseIBClick(Sender: TObject);
    procedure btnBrowseOutputFileClick(Sender: TObject);
    procedure chkOutputFileClick(Sender: TObject);
  private
    function GetIBFileName: string;
    function GetOutputType: TOutputType;
    function GetOutputFileName: string;
    function GetUseExisting: Boolean;
    procedure SetUseExisting(const Value: Boolean);
    function GetDeleteExisting: Boolean;
    procedure SetDeleteExisting(const Value: Boolean);
    procedure SetIBFileName(const Value: string);
    procedure SetOutputFileName(const Value: string);
    procedure SetOutputType(const Value: TOutputType);
  public
    property DeleteExisting: Boolean read GetDeleteExisting write SetDeleteExisting;
    property IBFileName: string read GetIBFileName write SetIBFileName;
    property OutputFileName: string read GetOutputFileName write SetOutputFileName;
    property OutputType: TOutputType read GetOutputType write SetOutputType;
    property UseExisting: Boolean read GetUseExisting write SetUseExisting;
  end;

implementation

{$R *.DFM}

procedure TSelectIBFrame.btnBrowseIBClick(Sender: TObject);
begin
  if dlgSelectIB.Execute then
    txtIB.Text := dlgSelectIB.FileName;
end;

procedure TSelectIBFrame.btnBrowseOutputFileClick(Sender: TObject);
begin
  if dlgSaveOutputFile.Execute then
    txtOutputFile.Text := dlgSaveOutputFile.FileName;
end;

procedure TSelectIBFrame.chkOutputFileClick(Sender: TObject);
var
  TargetIsIB: Boolean;
  TargetIsText: Boolean;
begin
  TargetIsText := chkOutputFile.Checked;
  TargetIsIB := not TargetIsText;

  txtIB.Enabled := TargetIsIB;
  btnBrowseIB.Enabled := TargetIsIB;
  chkDeleteExisting.Enabled := TargetIsIB;
  txtIBUserName.Enabled := TargetIsIB;
  txtIBPassword.Enabled := TargetIsIB;

  txtOutputFile.Enabled := TargetIsText;
  btnBrowseOutputFile.Enabled := TargetIsText;
end;

function TSelectIBFrame.GetIBFileName: string;
begin
  Result := txtIB.Text;
end;

function TSelectIBFrame.GetOutputType: TOutputType;
begin
  if chkOutputFile.Checked then
    Result := otText
  else
    Result := otIB;
end;

function TSelectIBFrame.GetOutputFileName: string;
begin
  Result := txtOutputFile.Text;
end;

function TSelectIBFrame.GetUseExisting: Boolean;
begin
  Result := chkUseExisting.Checked;
end;

procedure TSelectIBFrame.SetUseExisting(const Value: Boolean);
begin
  chkUseExisting.Checked := Value;
end;

function TSelectIBFrame.GetDeleteExisting: Boolean;
begin
  Result := chkDeleteExisting.Checked;
end;

procedure TSelectIBFrame.SetDeleteExisting(const Value: Boolean);
begin
  chkDeleteExisting.Checked := Value;
end;

procedure TSelectIBFrame.SetIBFileName(const Value: string);
begin
  txtIB.Text := Value;
end;

procedure TSelectIBFrame.SetOutputFileName(const Value: string);
begin
  txtOutputFile.Text := Value;
end;

procedure TSelectIBFrame.SetOutputType(const Value: TOutputType);
begin
  chkOutputFile.Checked := Value = otText;
end;

end.
