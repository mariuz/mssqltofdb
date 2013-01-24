
{*******************************************************}
{                                                       }
{     Base64 Library                                    }
{                                                       }
{     Provides routines to work with base64 encoding.   }
{                                                       }
{*******************************************************}

unit Base64Unit;

interface

function StringToBase64(const Text: string): string;

implementation

type
  TBase64Data = array [0..2] of Byte;
  TBase64Quantum = array [0..3] of Char;

const
  Base64Map: array[0..63] of Char =
    ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
     'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
     'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', { 00 - 25 }
     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
     'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
     's', 't', 'u', 'v', 'w', 'x', 'y', 'z'  { 26 - 51 },
     '0', '1', '2', '3', '4', '5', '6', '7', '8',
     '9', '+', '/');
  Base64Pad = '=';

procedure EncodeBaseQuantum(const BaseData: TBase64Data;
  var BaseQuantum: TBase64Quantum; const BytesUsed: Integer);
begin
  BaseQuantum[0] := Base64Map[BaseData[0] shr 2];
  BaseQuantum[1] := Base64Map[((BaseData[0] and $03) shl 4) or
                              (BaseData[1] shr 4)];
  BaseQuantum[2] := Base64Map[((BaseData[1] and $0F) shl 2) or
                              ((BaseData[2] and $C0) shr 6)];
  BaseQuantum[3] := Base64Map[BaseData[2] and $3F];
  if BytesUsed = 1 then
    BaseQuantum[2] := Base64Pad
  else if BytesUsed = 2 then begin
    BaseQuantum[2] := Base64Pad;
    BaseQuantum[1] := Base64Pad;
  end;
end;

function StringToBase64(const Text: string): string;
var
  BaseData: TBase64Data;
  BaseQuantum: TBase64Quantum;
  CharsLeft: Integer;
  CharsUsed: Integer;
  P: PChar;
begin
  { This Base64 encoding was written according to RFC 2045. }
  Result := '';
  CharsLeft := Length(Text);
  if CharsLeft = 0 then exit;
  P := PChar(Text);
  while CharsLeft > 0 do begin
    if CharsLeft < 3 then
      CharsUsed := CharsLeft
    else
      CharsUsed := 3;
    Move(P^, BaseData[0], CharsUsed);
    EncodeBaseQuantum(BaseData, BaseQuantum, CharsUsed);
    Result := Result + BaseQuantum;
    Dec(CharsLeft, CharsUsed);
    Inc(P, CharsUsed);
  end;
end;

end.
