{*****************************************************************************}
{                                                                             }
{ SQL2GDB Project                                                             }
{ Text Columns Editor Control                                                 }
{                                                                             }
{*****************************************************************************}

unit MLRTextColEditorUnit;

interface

uses
  Classes, Contnrs, Controls, Forms, Graphics, Messages, StdCtrls, Windows;

type
  TColumnLine = class
  private
    FColIndex: Integer;
    FColor: TColor;
  published
    property ColIndex: Integer read FColIndex write FColIndex;
    property Color: TColor read FColor write FColor;
  end;

  TMLRTextColEditor = class(TScrollingWinControl)
  private
    FCanvas: TControlCanvas;
    FColCount: Integer;
    FColumnLines: TObjectList;
    FStrings: TStringList;
    FColWidth: Integer;
    FEmptyLineColor: TColor;
    FLineHeight: Integer;
    FMultipleColors: Boolean;
    FMultipleColorIndex: Integer;
    FReadOnly: Boolean;
    function GetColLine(Index: Integer): TColumnLine;
    function GetColLineCount: Integer;
    function GetStrings: TStrings;
    procedure PaintCol(Col: TColumnLine); overload;
    procedure PaintCol(Col: TColumnLine; Color: TColor); overload;
    procedure PaintCol(ColIndex: Integer; Color: TColor); overload;
    procedure PaintColumnLines;
    procedure PaintTextLines;
    procedure RefreshMetrics;
    procedure SetEmptyLineColor(const Value: TColor);
    procedure SetStrings(const Value: TStrings);
    procedure StringsChange(Sender: TObject);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintWindow(DC: HDC); override;
    property Canvas: TControlCanvas read FCanvas;
    property ColumnLines: TObjectList read FColumnLines;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddColumnLine(ColIndex: Integer; Color: TColor);
    procedure Clear;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint); overload;
    procedure MouseToCell(var ACol, ARow: Longint); overload;
    procedure Paint;
    function RemoveColumnByIndex(ColIndex: Integer): Boolean;
    procedure SortColumns;
    property ColCount: Integer read FColCount;
    property ColLineCount: Integer read GetColLineCount;
    property ColLines[Index: Integer]: TColumnLine read GetColLine;
    property ColWidth: Integer read FColWidth;
    property EmptyLineColor: TColor read FEmptyLineColor write SetEmptyLineColor;
    property LineHeight: Integer read FLineHeight;
    property MultipleColors: Boolean read FMultipleColors write FMultipleColors;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Strings: TStrings read GetStrings write SetStrings;
    property OnMouseMove;
  end;

const
  MultipleColorPalette: array[0..9] of TColor = (
    clBlack, clBlue, clGreen, clRed, clFuchsia,
    clLime, clPurple, clTeal, clYellow, clOlive
  );

procedure ListTopLines(const FileName: string; const MaxLines: Integer;
  Strings: TStrings);

implementation

procedure ListTopLines(const FileName: string; const MaxLines: Integer;
  Strings: TStrings);
var
  F: TextFile;
  Index: Integer;
  Line: string;
begin
  Strings.BeginUpdate;
  try
    AssignFile(F, FileName);
    Reset(F);
    try
      Index := 0;
      while (Index <= MaxLines) and not Eof(F) do begin
        Readln(F, Line);
        Strings.Add(Line);
        Inc(Index);
      end;
    finally
      CloseFile(F);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

{ TMLRTextColEditor }

procedure TMLRTextColEditor.AddColumnLine(ColIndex: Integer;
  Color: TColor);
var
  Col: TColumnLine;
begin
  Col := TColumnLine.Create;
  ColumnLines.Add(Col);
  Col.ColIndex := ColIndex;
  Col.Color := Color;

  SetWindowOrgEx(Canvas.Handle, HorzScrollBar.Position,
    VertScrollBar.Position, nil);
  PaintCol(Col);
end;

procedure TMLRTextColEditor.Clear;
begin
  Strings.Clear;
  ColumnLines.Clear;
end;

procedure TMLRTextColEditor.Click;
var
  ColIndex: Integer;
  Color: TColor;
  MousePos: TPoint;
begin
  inherited;
  if ReadOnly then Exit;
  if ColWidth = 0 then Exit;
  MousePos := Mouse.CursorPos;
  MousePos := ScreenToClient(MousePos);
  ColIndex := (MousePos.X + HorzScrollBar.Position) div ColWidth;
  if not RemoveColumnByIndex(ColIndex) then begin
    if MultipleColors then begin
      Color := MultipleColorPalette[FMultipleColorIndex];
      FMultipleColorIndex := (FMultipleColorIndex + 1) mod High(MultipleColorPalette);
    end else
      Color := clBlack;
    AddColumnLine(ColIndex, Color);
  end;
end;

constructor TMLRTextColEditor.Create(AOwner: TComponent);
begin
  inherited;
  FColumnLines := TObjectList.Create(True);
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  FStrings := TStringList.Create;
  FStrings.OnChange := StringsChange;
  Color := clWhite;
  Brush.Color := clWhite;
  // BevelKind := bkFlat;
  Font.Name := 'Courier New';
  Font.Size := 10;
  FEmptyLineColor := clSilver;
  FMultipleColors := True;
  Width := 200;
  Height := 160;
end;

procedure TMLRTextColEditor.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_BORDER;
  Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
end;

destructor TMLRTextColEditor.Destroy;
begin
  FCanvas.Free;
  FColumnLines.Free;
  FStrings.Free;
  inherited;
end;

function TMLRTextColEditor.GetColLine(Index: Integer): TColumnLine;
begin
  Result := TColumnLine(ColumnLines[Index]);
end;

function TMLRTextColEditor.GetColLineCount: Integer;
begin
  Result := ColumnLines.Count;
end;

function TMLRTextColEditor.GetStrings: TStrings;
begin
  Result := FStrings;
end;

procedure TMLRTextColEditor.MouseToCell(X, Y: Integer; var ACol,
  ARow: Integer);
begin
  inherited;
  if (ColWidth = 0) or (LineHeight = 0) or (X < 0) or (Y < 0) then begin
    ACol := -1;
    ARow := -1;
    Exit;
  end;
  ACol := (X + HorzScrollBar.Position) div ColWidth;
  ARow := (Y + VertScrollBar.Position) div LineHeight;
end;

procedure TMLRTextColEditor.MouseToCell(var ACol, ARow: Integer);
begin
  MouseToCell(Mouse.CursorPos.X, Mouse.CursorPos.Y, ACol, ARow);
end;

procedure TMLRTextColEditor.Paint;
begin
  RefreshMetrics;
  SaveDC(Canvas.Handle);
  try
    { Offset the window for the scrollbars, and the viewport
      for the bevel. }
    SetWindowOrgEx(Canvas.Handle, HorzScrollBar.Position,
      VertScrollBar.Position, nil);
    SetViewportOrgEx(Canvas.Handle, 2, 2, nil);
    PaintTextLines;
    PaintColumnLines;
  finally
    RestoreDC(Canvas.Handle, -1);
  end;
end;

procedure TMLRTextColEditor.PaintCol(ColIndex: Integer; Color: TColor);
var
  X: Integer;
begin
  X := 2 + ColWidth * ColIndex;
  Canvas.Pen.Color := Color;
  Canvas.MoveTo(X, 4);
  Canvas.LineTo(X, 8 + Strings.Count * LineHeight);
end;

procedure TMLRTextColEditor.PaintCol(Col: TColumnLine; Color: TColor);
begin
  PaintCol(Col.ColIndex, Color);
end;

procedure TMLRTextColEditor.PaintCol(Col: TColumnLine);
begin
  PaintCol(Col.ColIndex, Col.Color);
end;

procedure TMLRTextColEditor.PaintColumnLines;
var
  Col: TColumnLine;
  i: Integer;
begin
  for i := 0 to ColCount do
    PaintCol(i, EmptyLineColor);
  for i := 0 to ColumnLines.Count - 1 do begin
    Col := ColumnLines[i] as TColumnLine;
    PaintCol(Col);
  end;
end;

procedure TMLRTextColEditor.PaintTextLines;
var
  i, j: Integer;
  Line: string;
  X, Y: Integer;
begin
  Y := 4;
  for i := 0 to Strings.Count - 1 do begin
    X := 4;
    Line := Strings[i];
    for j := 1 to Length(Line) do begin
      Canvas.TextOut(X, Y, Line[j]);
      Inc(X, ColWidth);
    end;
    Inc(Y, LineHeight);
  end;
end;

procedure TMLRTextColEditor.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TMLRTextColEditor.RefreshMetrics;
begin
  Canvas.Font := Font;
  FColWidth := Canvas.TextWidth('W') + 2;
  FLineHeight := Canvas.TextHeight('Wj');
  HorzScrollBar.Range := ColCount * ColWidth + 4;
  VertScrollBar.Range := LineHeight * Strings.Count + 8;
end;

function TMLRTextColEditor.RemoveColumnByIndex(ColIndex: Integer): Boolean;
var
  Col: TColumnLine;
  i: Integer;
begin
  for i := 0 to ColumnLines.Count - 1 do begin
    Col := ColumnLines[i] as TColumnLine;
    if Col.ColIndex = ColIndex then begin
      SetWindowOrgEx(Canvas.Handle, HorzScrollBar.Position,
        VertScrollBar.Position, nil);
      PaintCol(Col, EmptyLineColor);
      ColumnLines.Delete(i);
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TMLRTextColEditor.SetEmptyLineColor(const Value: TColor);
begin
  if FEmptyLineColor <> Value then begin
    FEmptyLineColor := Value;
    Invalidate;
  end;
end;

procedure TMLRTextColEditor.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
end;

function CompareColumns(Item1, Item2: Pointer): Integer;
var
  C1, C2: TColumnLine;
begin
  C1 := Item1; C2 := Item2;
  Result := C1.ColIndex - C2.ColIndex;
end;

procedure TMLRTextColEditor.SortColumns;
begin
  FColumnLines.Sort(CompareColumns);
end;

procedure TMLRTextColEditor.StringsChange(Sender: TObject);
var
  i: Integer;
begin
  FColCount := 0;
  for i := 0 to Strings.Count - 1 do begin
    if Length(Strings[i]) > ColCount then
      FColCount := Length(Strings[i]);
  end;
  Invalidate;
end;

procedure TMLRTextColEditor.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

end.
