{*****************************************************************************}
{                                                                             }
{ Text Delimiter Frame Unit                                                   }
{ Provides a frame to allow the user to select text delimiter and other       }
{ schema options.                                                             }
{                                                                             }
{*****************************************************************************}

unit TextDelimiterFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, TextModelUnit, MLRTextColEditorUnit;

type
  TTextDelimiterFrame = class(TFrame, ITextInfoSupport)
    Label1: TLabel;
    grpLayout: TRadioGroup;
    grpDelimiter: TRadioGroup;
    chkFirstRowHasNames: TCheckBox;
    procedure grpLayoutClick(Sender: TObject);
  private
    FEditor: TMLRTextColEditor;
    FTextInfo: TTextInfo;
    procedure PrepareEditor;
  protected
    { ITextInfoSupport implementation. }
    function GetTextInfo: TTextInfo;
    procedure SetTextInfo(Value: TTextInfo);
  public
    destructor Destroy; override;
    procedure ReadFromTextInfo;
    procedure WriteToTextInfo;
    property TextInfo: TTextInfo read FTextInfo write FTextInfo;
  end;

implementation

{$R *.DFM}

const
  DelimitedIndex = 0;
  FixedIndex = 1;

  TabIndex = 0;
  SpaceIndex = 1;
  SemicolonIndex = 2;
  CommaIndex = 3;
  DashIndex = 4;

resourcestring
  sTwoColsRequired = 'At least two column lines are required to define a field';    

{ TTextDelimiterFrame }

function TTextDelimiterFrame.GetTextInfo: TTextInfo;
begin
  Result := FTextInfo;
end;

procedure TTextDelimiterFrame.SetTextInfo(Value: TTextInfo);
begin
  FTextInfo := Value;
end;

procedure TTextDelimiterFrame.grpLayoutClick(Sender: TObject);
begin
  grpDelimiter.Visible := grpLayout.ItemIndex = DelimitedIndex;
  FEditor.Visible := not grpDelimiter.Visible;
end;

procedure TTextDelimiterFrame.ReadFromTextInfo;
var
  ColInfo: TColInfo;
  i: Integer;
  StartPos: Integer;
begin
  PrepareEditor;
  FEditor.Clear;
  ListTopLines(TextInfo.TextFileName, 64, FEditor.Strings);
  if TextInfo.SchemaColumns.Count = 0 then begin
    FEditor.AddColumnLine(0, clBlack);
    FEditor.AddColumnLine(FEditor.ColCount, clBlack);
  end else begin
    StartPos := 0;
    FEditor.AddColumnLine(StartPos, clBlack);
    for i := 0 to TextInfo.SchemaColumns.Count - 1 do begin
      ColInfo := StrToColInfo(TextInfo.SchemaColumns[i]);
      if ColInfo.Width > 0 then begin
        Inc(StartPos, ColInfo.Width);
        FEditor.AddColumnLine(StartPos, clBlack);
      end;
    end;
  end;
  if TextInfo.DataLayout = dlDelimited then
    grpLayout.ItemIndex := DelimitedIndex
  else
    grpLayout.ItemIndex := FixedIndex;
  grpLayoutClick(Self);
  case TextInfo.Separator of
    fsSpace:
      grpDelimiter.ItemIndex := SpaceIndex;
    fsTab:
      grpDelimiter.ItemIndex := TabIndex;
    fsComma:
      grpDelimiter.ItemIndex := CommaIndex;
    fsSemiColon:
      grpDelimiter.ItemIndex := SemiColonIndex;
    fsDash:
      grpDelimiter.ItemIndex := DashIndex;
  end;
  chkFirstRowHasNames.Checked := TextInfo.FirstRowHasNames;
end;

procedure TTextDelimiterFrame.WriteToTextInfo;
var
  FieldStart: Integer;
  i: Integer;
begin
  if grpLayout.ItemIndex = DelimitedIndex then begin
    TextInfo.DataLayout := dlDelimited;
    case grpDelimiter.ItemIndex of
      SpaceIndex:
        TextInfo.Separator := fsSpace;
      TabIndex:
        TextInfo.Separator := fsTab;
      CommaIndex:
        TextInfo.Separator := fsComma;
      SemiColonIndex:
        TextInfo.Separator := fsSemiColon;
      DashIndex:
        TextInfo.Separator := fsDash;
    end;
  end else begin
    TextInfo.DataLayout := dlFixedWidth;
    FEditor.SortColumns;
    if FEditor.ColLineCount < 2 then
      raise Exception.Create(sTwoColsRequired);
    TextInfo.SchemaColumns.Clear;
    FieldStart := FEditor.ColLines[0].ColIndex;
    for i := 1 to FEditor.ColLineCount - 1 do begin
      TextInfo.SchemaColumns.Add(
        Format('Col%d Char Width %d', [i,
        FEditor.ColLines[i].ColIndex - FieldStart]));
      FieldStart := FEditor.ColLines[i].ColIndex;
    end;
  end;
  TextInfo.FirstRowHasNames := chkFirstRowHasNames.Checked;
end;

destructor TTextDelimiterFrame.Destroy;
begin
  FEditor.Free;
  inherited;
end;

procedure TTextDelimiterFrame.PrepareEditor;
begin
  if FEditor = nil then begin
    FEditor := TMLRTextColEditor.Create(Self);
    FEditor.Parent := Self;
    FEditor.Top := grpDelimiter.Top;
    FEditor.Width := ClientWidth;
    FEditor.Height := ClientHeight - FEditor.Top;
    FEditor.Anchors := [akTop, akRight, akLeft, akBottom];
  end;
end;

end.
