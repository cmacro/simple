unit uTransformBmps;

interface

uses
  Windows, Graphics;

type
  TTransformationKind = (tkDingy, tkDirty, tkFade, tkMakeMask);

// Õº∆¨¥¶¿Ì
function TransformBitmap(ABmp: TBitmap; AKind: TTransformationKind):Boolean;

implementation

type
  TRGBColors = array of TRGBQuad;

  TColorTransitionMap = record
    RedScale: Single;
    GreenScale: Single;
    BlueScale: Single;
    SrcAlpha: Byte;
    SrcConstantAlpha: Byte;
  end;

  TTransformationFun = procedure(var AColor: TRGBQuad);

const
  CLR_NONE: TRGBQuad = (rgbBlue: $FF; rgbGreen: $FF; rgbRed: $FF; rgbReserved: $FF);
  CLR_TRANSPARENT: TRGBQuad = (rgbBlue: 0; rgbGreen: 0; rgbRed: 0; rgbReserved: 0);

  DisableMap: TColorTransitionMap = (RedScale: 0.0729; GreenScale: 0.7146; BlueScale: 0.2125; SrcAlpha: 105; SrcConstantAlpha: 151);
  FadeMap: TColorTransitionMap = (RedScale: 0.299; GreenScale: 0.587; BlueScale: 0.114; SrcAlpha: 192; SrcConstantAlpha: 64);
  GrayMap: TColorTransitionMap = (RedScale: 0.299; GreenScale: 0.587; BlueScale: 0.114; SrcAlpha: $FF; SrcConstantAlpha: $FF);


procedure FillInfo(out AHeader: TBitmapInfoHeader; AWidth, AHeight: Integer; ATopDownDIB: WordBool);
begin
  ZeroMemory(@AHeader, SizeOf(AHeader));
  AHeader.biSize := SizeOf(TBitmapInfoHeader);
  AHeader.biWidth := AWidth;
  if ATopDownDIB then
    AHeader.biHeight := -AHeight
  else
    AHeader.biHeight := AHeight;
  AHeader.biPlanes := 1;
  AHeader.biBitCount := 32;
  AHeader.biCompression := BI_RGB;
end;

function GetDIB(ABmp: TBitmap; const AColors: TRGBColors; ATopDownDIB: WordBool): Boolean;
var
  DC: HDC;
  rInfo: TBitmapInfo;
  I: Integer;
  bScanLine: boolean;
  pData: Pointer;
begin
  FillInfo(rInfo.bmiHeader, ABmp.Width, ABmp.Height, ATopDownDIB);
  DC := CreateCompatibleDC(0);
  try
    Result := GetDIBits(DC, ABmp.Handle, 0, ABmp.Height, AColors, rInfo, DIB_RGB_COLORS) <> 0;
    if not Result then
    begin
      Result := True;
      for I := 0 to ABmp.Height - 1 do
      begin
        bScanLine := GetDIBits(DC, ABmp.Handle, I, 1, @AColors[ABmp.Width * I], rInfo, DIB_RGB_COLORS) <> 0;
        if not bScanLine then
        begin
          // AllocMem
          pData := GetMemory(ABmp.Width * SizeOf(TRGBQuad));
          try
            bScanLine := GetDIBits(DC, ABmp.Handle, I, 1, pData, rInfo, DIB_RGB_COLORS) <> 0;
            Move(pData^, AColors[ABmp.Width * I], ABmp.Width * SizeOf(TRGBQuad));
          finally
            FreeMem(pData);
          end;
        end;
        Result := Result and bScanLine;
      end;
    end;
  finally
    DeleteDC(DC);
  end;
end;

function SetDIB(ABmp: TBitmap; const AColors: TRGBColors; ATopDownDIB: WordBool): Boolean;
var
  DC: HDC;
  rInfo: TBitmapInfo;
begin
  FillInfo(rInfo.bmiHeader, ABmp.Width, ABmp.Height, ATopDownDIB);
  DC := CreateCompatibleDC(0);
  try
    Result := SetDIBits(DC, ABmp.Handle, 0, ABmp.Height, AColors, rInfo, DIB_RGB_COLORS) <> 0;
  finally
    DeleteDC(DC);
  end;
end;

function IsColorTransparent(const AColor: TRGBQuad): Boolean; inline;
begin
  Result := DWORD(AColor) = DWORD(CLR_TRANSPARENT);
end;

function GetChannelValue(AValue: Integer): Byte; inline;
begin
  if AValue < 0 then
    Result := 0
  else
    if AValue > 255 then
      Result := 255
    else
      Result := AValue;
end;

procedure Dingy(var AColor: TRGBQuad);
begin
  with AColor do
    if not IsColorTransparent(AColor) then
    begin
      if rgbReserved = $FF then
      begin
        rgbRed := GetChannelValue(rgbRed + MulDiv(255 - rgbRed, 3, 10));
        rgbGreen := GetChannelValue(rgbGreen + MulDiv(255 - rgbGreen, 3, 10));
        rgbBlue := GetChannelValue(rgbBlue + MulDiv(255 - rgbBlue, 3, 10));
      end
      else
      begin
        rgbRed     := GetChannelValue(MulDiv(rgbRed, 200, 255));
        rgbGreen   := GetChannelValue(MulDiv(rgbGreen, 200, 255));
        rgbBlue    := GetChannelValue(MulDiv(rgbBlue, 200, 255));
        rgbReserved:= GetChannelValue(MulDiv(rgbReserved, 200, 255));
      end;
    end;
end;

type
  TRGBA = packed record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;

function ColorToRGBQuad(AColor: TColor; AReserved: Byte = 0): TRGBQuad;
var
  Tmp: TRGBA;
begin
  DWORD(Tmp) := ColorToRGB(AColor);
  Result.rgbBlue := Tmp.B;
  Result.rgbRed := Tmp.R;
  Result.rgbGreen := Tmp.G;
  Result.rgbReserved := AReserved;
end;

function RGBQuadToColor(const ARGB: TRGBQuad): TColor;
var
  tmp: TRGBA;
begin
  tmp.B := ARGB.rgbBlue;
  tmp.R := ARGB.rgbRed;
  tmp.G := ARGB.rgbGreen;
  tmp.A := ARGB.rgbReserved;
  Result := DWORD(tmp);
end;

procedure BlendColor(const ASource: TRGBQuad; var ADest: TRGBQuad; ASourceConstantAlpha: Byte);

  function GetValue(AValue: Single): Byte;
  begin
    Result := GetChannelValue(Round(AValue));
  end;

var
  iSca, iAlpha: Single;
begin
  iSca := ASourceConstantAlpha / 255;
  iAlpha := 1 - ASource.rgbReserved * iSca / 255;

  ADest.rgbRed := GetValue(ASource.rgbRed * iSca + iAlpha * ADest.rgbRed);
  ADest.rgbGreen := GetValue(ASource.rgbGreen * iSca + iAlpha * ADest.rgbGreen);
  ADest.rgbBlue := GetValue(ASource.rgbBlue * iSca + iAlpha * ADest.rgbBlue);
  ADest.rgbReserved := GetValue(ASource.rgbReserved * iSca + iAlpha * ADest.rgbReserved);
end;

procedure ScaleColor(var AColor: TRGBQuad; const AColorMap: TColorTransitionMap);
var
  iValue: Byte;
begin
  iValue := GetChannelValue(Round(AColorMap.RedScale * AColor.rgbRed + AColorMap.GreenScale * AColor.rgbGreen + AColorMap.BlueScale * AColor.rgbBlue));

  AColor.rgbBlue := iValue;
  AColor.rgbGreen := iValue;
  AColor.rgbRed := iValue;
end;

procedure Dirty(var AColor: TRGBQuad);
var
  rScreen:TRGBQuad;
begin
  if not IsColorTransparent(AColor) then
  begin
    ScaleColor(AColor, GrayMap);

    rScreen := ColorToRGBQuad(clBtnShadow);
    rScreen.rgbReserved := $C0;

    BlendColor(rScreen, AColor, $EE);
  end;
end;

procedure Fade(var AColor: TRGBQuad);
begin
  if not IsColorTransparent(AColor) then
    ScaleColor(AColor, FadeMap);
end;

procedure MakeMask(var AColor: TRGBQuad);
begin
  if IsColorTransparent(AColor) then
    AColor := CLR_NONE
  else
    AColor := CLR_TRANSPARENT;
end;

function TransformBitmap(ABmp: TBitmap; AKind: TTransformationKind):Boolean;
const
  TRANSFORMFUNS: array [TTransformationKind] of TTransformationFun = (Dingy, Dirty, Fade, MakeMask);
var
  rColors: TRGBColors;
  I, j: Integer;
  pFun: TTransformationFun;
begin
  Result := False;
  if ABmp.Width * ABmp.Height <> 0 then
  begin
    pFun := TRANSFORMFUNS[AKind];
    SetLength(rColors, ABmp.Width * ABmp.Height);
    GetDIB(ABmp, rColors, False);

    with ABmp do
      for I := 0 to Width - 1 do
        for J := 0 to Height - 1 do
          pFun(rColors[J * Width + I]);

    SetDIB(ABmp, rColors, False);
    Result := True;
  end;
end;


end.
