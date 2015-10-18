//
//  uUIShowings.pas
//  TestAnim
//    子控件切换显示时渐变的动态效果
//
//  Created by SunSeed on 14-11-1.
//  Copyright (c) 2014年 SunSeed. All rights reserved.
//

unit uUIShowings;

interface

uses
  Classes, Messages, Controls, Types, windows, Graphics, Forms, SysUtils;


procedure PrepareForAnimation(AHideView: TWinControl);
procedure PrepareAnimScreen(AView:TWinControl);
procedure AnimShowControl(AView: TWinControl; ATime: word = 500);

implementation

type
  TacWinControl = class(TWinControl);

  TMaskWindow = class(TCustomControl)
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Message: TWmEraseBkgnd); message WM_NCPAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  end;

  // 用于扫描bmp像素
  TRGBA = packed record
    B: Byte;
    G: Byte;
    R: Byte;
    A: Byte;
  end;
  PRGBAArray = ^TRGBAArray;
  TRGBAArray = array [0 .. MAXWORD] of TRGBA;

const
  TimerInterval = 12;

var
  AnimBmp: TBitmap = nil;
  MaskWindow: TMaskWindow = nil;


function acLayered: boolean;
begin
  Result := False;
end;

function max(v1, v2: integer):integer;
begin
  Result := v1;
  if v2 > v1 then
    Result := v2;
end;

procedure SetChildOrderAfter(Child: TWinControl; Control: TControl);
var
  I: Integer;
begin
  for I := 0 to Child.Parent.ControlCount do
    if Child.Parent.Controls[I] = Control then
    begin
      TacWinControl(Child.Parent).SetChildOrder(Child, I + 1);
      break;
    end;
end;

function CreateBmp32(const AWidth, AHeight: integer): TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.HandleType := bmDIB;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

procedure SumBitmapsByMask(var Outbmp, Src1, Src2: TBitmap; MaskBmp: TBitmap; Percent: word = 0);
var
  S1, S2, M: PRGBAArray;
  R: PRGBAArray;
  X, Y, w, h: integer;
begin
  if (Src1.Width <> Src2.Width) or (Src1.Height <> Src2.Height) then
    Exit;
  w := Src1.Width - 1;
  h := Src1.Height - 1;
  if MaskBmp = nil then
    for Y := 0 to h do
    begin
      S1 := Src1.ScanLine[Y];
      S2 := Src2.ScanLine[Y];
      R := Outbmp.ScanLine[Y];
      for X := 0 to w do
      begin
        R[X].R := (((S1[X].R - S2[X].R) * Percent + S2[X].R shl 8) shr 8) and MaxByte;
        R[X].G := (((S1[X].G - S2[X].G) * Percent + S2[X].G shl 8) shr 8) and MaxByte;
        R[X].B := (((S1[X].B - S2[X].B) * Percent + S2[X].B shl 8) shr 8) and MaxByte;
      end
    end
  else
    for Y := 0 to h do
    begin
      S1 := Src1.ScanLine[Y];
      S2 := Src2.ScanLine[Y];
      R := Outbmp.ScanLine[Y];
      M := MaskBmp.ScanLine[Y];
      for X := 0 to w do
      begin
        R[X].R := (((S1[X].R - S2[X].R) * M[X].R + S2[X].R shl 8) shr 8) and MaxByte;
        R[X].G := (((S1[X].G - S2[X].G) * M[X].G + S2[X].G shl 8) shr 8) and MaxByte;
        R[X].B := (((S1[X].B - S2[X].B) * M[X].B + S2[X].B shl 8) shr 8) and MaxByte;
      end
    end
end;

procedure PrepareForAnimation(AHideView: TWinControl);
var
  cParent: TWinControl;
  Flags: Cardinal;
  R: TRect;
  hScrDC: hdc;
begin
  GetWindowRect(AHideView.Handle, R);
  if AnimBmp = nil then
    AnimBmp := CreateBmp32(AHideView.Width, AHideView.Height);

  hScrDC := GetDC(0);
  BitBlt(AnimBmp.Canvas.Handle, 0, 0, AHideView.Width, AHideView.Height, hScrDC, R.Left, R.Top, SRCCOPY);
  ReleaseDC(0, hScrDC);

  if MaskWindow = nil then
    MaskWindow := TMaskWindow.Create(AHideView);

  cParent := AHideView.Parent;
  if cParent <> nil then
  begin
    MaskWindow.Parent := cParent;
    MaskWindow.BoundsRect := AHideView.BoundsRect;
    MaskWindow.SetZOrder(True);
  end
  else
    MaskWindow.BoundsRect := R;

  BitBlt(MaskWindow.Canvas.Handle, 0, 0, AHideView.Width, AHideView.Height, AnimBmp.Canvas.Handle, 0, 0, SRCCOPY);
  if MaskWindow.Parent = nil then
  begin
    Flags := SWP_NOZORDER or SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE;
    SetWindowPos(MaskWindow.Handle, GetWindow(TWinControl(AHideView).Handle, GW_HWNDPREV), 0, 0, 0, 0, Flags);
  end
  else
    ShowWindow(MaskWindow.Handle, SW_SHOWNA);
end;

procedure AnimShowControl(AView: TWinControl; ATime: word);
var
  cMixbmp: TBitmap;
  cDestbmp: TBitmap;
  SavedDC, hPaintDC: hdc;
  I, iStepCount, prc: integer;
  iPercent: integer;
begin
  if MaskWindow = nil then PrepareForAnimation(AView);
  if MaskWindow = nil then Exit;

  // 获取目标显示界面
  cDestbmp := CreateBmp32(AView.Width, AView.Height);
  cDestbmp.Canvas.Lock;
  SavedDC := SaveDC(cDestbmp.Canvas.Handle);
  SendMessage(AView.Handle, WM_ERASEBKGND, WPARAM(cDestbmp.Canvas.Handle), 0);
  SendMessage(AView.Handle, WM_PAINT, WPARAM(cDestbmp.Canvas.Handle), 0);
  RestoreDC(cDestbmp.Canvas.Handle, SavedDC);
  cDestbmp.Canvas.UnLock;

  //
  cMixbmp := CreateBmp32(AView.Width, AView.Height);
  iStepCount := ATime div TimerInterval;

  hPaintDC := GetWindowDC(MaskWindow.Handle);
  if iStepCount > 0 then
  begin
    prc := MaxByte div iStepCount;
    iPercent := MaxByte;
    I := 0;
    while I <= iStepCount do
    begin
      SumBitmapsByMask(cMixbmp, AnimBmp, cDestbmp, nil, max(0, iPercent));
      BitBlt(hPaintDC, 0, 0, AView.Width, AView.Height, cMixbmp.Canvas.Handle, 0, 0, SRCCOPY);
      inc(I);
      dec(iPercent, prc);
      if (I > iStepCount) then
        break;
      if iStepCount > 0 then
        Sleep(TimerInterval);
    end;
  end;
  BitBlt(hPaintDC, 0, 0, AView.Width, AView.Height, cDestbmp.Canvas.Handle, 0, 0, SRCCOPY);

  if AView.Visible then
  begin
    SendMessage(AView.Handle, WM_SETREDRAW, 1, 0); // Vista
    if Win32MajorVersion >= 6 then
      RedrawWindow(AView.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_ALLCHILDREN or RDW_INVALIDATE or RDW_UPDATENOW);
    if not(AView is TCustomForm) or (dword(GetWindowLong(AView.Handle, GWL_EXSTYLE)) and WS_EX_LAYERED <> WS_EX_LAYERED) then
      SetWindowPos(MaskWindow.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_HIDEWINDOW or SWP_NOREDRAW or { SWP_NOCOPYBITS or } SWP_NOACTIVATE);
  end;

  ReleaseDC(MaskWindow.Handle, hPaintDC);
  FreeAndnil(MaskWindow);
  FreeAndnil(cMixbmp);
  FreeAndnil(AnimBmp);
  FreeAndnil(cDestbmp);
end;

procedure PrepareAnimScreen(AView:TWinControl);
begin
  if (MaskWindow <> nil) then
    TacWinControl(MaskWindow).SetZOrder(True);
end;

{ TMaskWindow }

constructor TMaskWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := False;
  Color := clBlack;
end;

procedure TMaskWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if (Parent = nil) and (ParentWindow = 0) then
    begin
      Params.Style := WS_POPUP;
      if (Owner is TWinControl) and
         ((dword(GetWindowLong(TWinControl(Owner).Handle, GWL_EXSTYLE)) and WS_EX_TOPMOST) <> 0) then
        Params.ExStyle := ExStyle or WS_EX_TOPMOST;
      WndParent := Application.Handle;
    end;
  end;
end;

procedure TMaskWindow.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TMaskWindow.WMNCPaint(var Message: TWmEraseBkgnd);
begin
  Message.Result := 0;
end;

end.
