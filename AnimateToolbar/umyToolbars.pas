unit umyToolbars;

interface

uses
  Classes, Windows, Messages, Controls, Actions, ImgList, Graphics, ActnList, Forms,
  Menus, SysUtils, Types;

type
  TSkinIndicator = (
    siInactive, siHover, siPressed, siSelected, siHoverSelected
    );

  TmyToolItem = record
    Action: TBasicAction;
    Enabled: boolean;
    Visible: boolean;
    ImageIndex: Integer;      // 考虑到标题功能图标和实际工具栏功能使用不同图标情况，分开图标索引
    Width: Word;              // 实际占用宽度，考虑后续加不同的按钮样式使用
    Fade: Word;               // 褪色量 0 - 255
    SaveEvent: TNotifyEvent;  // 原始的Action OnChange事件
  end;

  TmyCustomToolbar = class(TWinControl)
  private
    FAutoWidth: Boolean;
    FItems: array of TmyToolItem;
    FCount: Integer;
    FImages: TCustomImageList;

    FHotIndex: Integer;
    FPressedIndex: Integer;

    function HitTest(x, y: Integer): Integer;
    procedure ExecAction(Index: Integer);

    procedure DoOnActionChange(Sender: TObject);
    function  LoadActionIcon(Idx: Integer; AImg: TBitmap): boolean;
    procedure SetAutoWidth(const Value: Boolean);
    procedure SetHotIndex(const Value: Integer);
    procedure UpdateFade;

    procedure WMEraseBkgnd(var message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var message: TWMPaint); message WM_Paint;
    procedure WMMouseLeave(var message: TMessage); message WM_MOUSELEAVE;
    procedure WMMouseMove(var message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMTimer(var message: TWMTimer); message WM_TIMER;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    function  GetActualWidth: Integer;
  protected
    // 计算实际占用尺寸
    function CalcSize: TRect;
    procedure UpdateSize;

    procedure MouseMove(Shift: TShiftState; x: Integer; y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x: Integer; y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x: Integer; y: Integer); override;
    procedure PaintBackground(DC: HDC);
    procedure PaintWindow(DC: HDC); override;

  public
    procedure Add(Action: TBasicAction; AImageIndex: Integer = -1);
    function IndexOf(Action: TBasicAction): Integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth;
    property HotIndex: Integer read FHotIndex write SetHotIndex;
    property Images: TCustomImageList read FImages write FImages;
    property ActualWidth: Integer read GetActualWidth;

  end;

  TmyToolbar = class(TmyCustomToolbar)
  published
    property Color;
  end;

  UISkin = class
  private
    class procedure DrawTransparentBitmap(Source: TBitmap; sx, sy: Integer;
        Destination: HDC; const dX, dY: Integer; w, h: Integer; const Opacity: Byte); overload;
  public
    class procedure DrawButtonState(DC: HDC; AState: TSkinIndicator; const R:TRect; const AOpacity: Byte); static;
    class procedure DrawIcon(DC: HDC; R: TRect; ASrc: TBitmap; const Opacity: Byte = 255);
  end;


implementation


CONST
  TIMID_FADE = 1; // Action褪色

type
  TacAction = class(TBasicAction);




procedure TmyCustomToolbar.Add(Action: TBasicAction; AImageIndex: Integer);
begin
  if FCount >= Length(FItems) then
    SetLength(FItems, FCount + 5);

  ZeroMemory(@FItems[FCount], SizeOf(TmyToolItem));
  FItems[FCount].Action := Action;
  FItems[FCount].Enabled := true;
  FItems[FCount].Visible := true;
  FItems[FCount].ImageIndex := AImageIndex;
  FItems[FCount].Width := 20;
  FItems[FCount].Fade := 0;
  FItems[FCount].SaveEvent := TacAction(Action).OnChange;
  TacAction(Action).OnChange := DoOnActionChange;

  // 初始化状态
  with FItems[FCount] do
    if Action.InheritsFrom(TContainedAction) then
    begin
      Enabled := TContainedAction(Action).Enabled;
      Visible := TContainedAction(Action).Visible;
    end;

  inc(FCount);

  UpdateSize;
end;

function TmyCustomToolbar.CalcSize: TRect;
const
  SIZE_SPLITER = 10;
  SIZE_POPMENU = 10;
  SIZE_BUTTON = 20;
var
  w, h: Integer;
  I: Integer;
begin
  ///
  /// 占用宽度
  /// 如果考虑比较复杂的按钮样式和显示标题等功能，那么需要计算每个按钮实际占用宽度才能获得。

  // w := SIZE_SPLITER * 2 + SIZE_POPMENU;
  w := 0;
  for I := 0 to FCount - 1 do
    if FItems[i].Visible then
      w := w + FItems[I].Width;
  h := SIZE_BUTTON;
  Result := Rect(0, 0, w, h);
end;

procedure TmyCustomToolbar.CMHintShow(var Message: TCMHintShow);
var
  Idx: Integer;
  sHint: string;
  sTitle, sRemark, sShortCut: string;
begin
  sTitle := '';
  sRemark := '';
  sShortCut := '';
  Idx := FHotIndex;
  if (Idx >= FCount) or (not FItems[idx].Visible) then
    Idx := -1;

  // get hint data
  if Idx >= 0 then
  begin
    if FItems[Idx].Action.InheritsFrom(TContainedAction) then
      with TContainedAction(FItems[Idx].Action) do
      begin
        sTitle := Caption;
        sRemark := Hint;
        if ShortCut <> scNone then
          sShortCut := ShortCutToText(TCustomAction(Action).ShortCut);
      end;
  end;

  /// format hint string
  if sTitle <> ''  then
  begin
    if sShortCut = '' then
      sHint := sTitle
    else
      sHint := Format('%s（%s）', [sTitle, sShortCut]);

    if (sRemark <> '') and not SameText(sRemark, sTitle) then
      sHint := Format('%s'#13#10'  %s', [sHint, sRemark]);
  end
  else
    sHint := sRemark;

  Message.HintInfo.HintStr := sHint;
  if sHint = '' then
    Message.Result := 1;
end;

constructor TmyCustomToolbar.Create(AOwner: TComponent);
begin
  inherited;
  inherited Height := 20;
  inherited Width := 20 * 3;
  FHotIndex := -1;
  FPressedIndex := -1;
  FAutoWidth := true;
end;

destructor TmyCustomToolbar.Destroy;
begin
  if HandleAllocated  then
    KillTimer(Handle, TIMID_FADE);

  inherited;
end;

procedure TmyCustomToolbar.DoOnActionChange(Sender: TObject);
var
  Idx: Integer;
  bResize: boolean;
begin
  if Sender is TBasicAction then
  begin
    Idx := IndexOf(TBasicAction(Sender));
    if (Idx >= 0) and (Idx < FCount) then
    begin
      ///
      /// 外部状态改变响应
      ///
      if FItems[Idx].Action.InheritsFrom(TContainedAction) then
      begin
        FItems[Idx].Enabled := TContainedAction(Sender).Enabled;
        bResize := FItems[Idx].Visible <> TContainedAction(Sender).Visible;
        if bResize then
        begin
          FItems[Idx].Visible := not FItems[Idx].Visible;
          UpdateSize;
        end
        else if FItems[Idx].Visible then
          Invalidate;
      end;

      /// 执行原有事件
      if Assigned(FItems[Idx].SaveEvent) then
        FItems[Idx].SaveEvent(Sender);
    end;
  end;
end;

procedure TmyCustomToolbar.ExecAction(Index: Integer);
begin
  ///
  /// 执行命令
  ///
  if (Index >= 0) and (Index < FCount) then
    FItems[Index].Action.Execute;
end;

function TmyCustomToolbar.GetActualWidth: Integer;
var
  R: TRect;
begin
  R := CalcSize;
  Result := r.Width;
end;

function TmyCustomToolbar.HitTest(x, y: Integer): Integer;
var
  I: Integer;
  Idx: Integer;
  iOffx: Integer;
begin
  Idx := -1;
  iOffx := 0;
  if PtInRect(ClientRect, Point(x, y)) then
    for I := 0 to FCount - 1 do
    begin
      if not FItems[I].Visible then
        Continue;

      iOffx := iOffx + FItems[I].Width;
      if (iOffx > x) then
      begin
        Idx := I;
        Break;
      end;
    end;

  // 去除无效的按钮
  if (Idx >= 0) and (not FItems[Idx].Visible or not FItems[Idx].Enabled) then
    Idx := -1;

  Result := Idx;
end;

function TmyCustomToolbar.IndexOf(Action: TBasicAction): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if FItems[I].Action = Action then
    begin
      Result := I;
      Break;
    end;
end;

function TmyCustomToolbar.LoadActionIcon(Idx: Integer; AImg: TBitmap): boolean;

  function LoadIcon(AImgs: TCustomImageList; AIndex: Integer): boolean;
  begin
    Result := False;
    if Assigned(AImgs) and (AIndex >= 0) and (AIndex < AImgs.Count) then
      Result := AImgs.GetBitmap(AIndex, AImg);
  end;

var
  bHasImg: boolean;
  ImgIdx: Integer;

begin
  /// 获取Action的图标
  ImgIdx := -1;
  AImg.Canvas.Brush.Color := clBlack;
  AImg.Canvas.FillRect(Rect(0, 0, AImg.Width, AImg.Height));
  bHasImg := LoadIcon(FImages, FItems[Idx].ImageIndex);
  if not bHasImg and (FItems[Idx].Action is TCustomAction) then
  begin
    ImgIdx := TCustomAction(FItems[Idx].Action).ImageIndex;
    bHasImg := LoadIcon(TCustomAction(FItems[Idx].Action).Images, ImgIdx);
  end;
  if not bHasImg then
    bHasImg := LoadIcon(FImages, ImgIdx);

  Result := bHasImg;
end;

procedure TmyCustomToolbar.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Integer);
begin
  if mbLeft = Button then
  begin
    FPressedIndex := HitTest(x, y);
    Invalidate;
  end;
end;

procedure TmyCustomToolbar.MouseMove(Shift: TShiftState; x, y: Integer);
begin
end;

procedure TmyCustomToolbar.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Integer);
var
  iPressed: Integer;
begin
  if FPressedIndex >= 0 then
  begin
    iPressed := HitTest(x, y);
    if iPressed = FPressedIndex then
      ExecAction(iPressed);
  end;
  FPressedIndex := -1;
  Invalidate;
end;

procedure TmyCustomToolbar.PaintBackground(DC: HDC);
var
  hB: HBRUSH;
  R: TRect;
begin
  R := GetClientRect;
  hB := CreateSolidBrush(ColorToRGB(Color));
  FillRect(DC, R, hB);
  DeleteObject(hB);
end;

procedure TmyCustomToolbar.PaintWindow(DC: HDC);
  function GetActionState(Idx: Integer): TSkinIndicator;
  begin
    Result := siInactive;
    if (Idx = FPressedIndex) then
      Result := siPressed
    else if (Idx = FHotIndex) and (FPressedIndex = -1) then
      Result := siHover;
  end;

var
  cIcon: TBitmap;
  R: TRect;
  I: Integer;
  iOpacity: byte;
begin
  R := Rect(0, 0, 0, ClientHeight);

  /// 绘制Button
  cIcon := TBitmap.Create;
  cIcon.PixelFormat := pf32bit;
  cIcon.alphaFormat := afIgnored;
  for I := 0 to FCount - 1 do
  begin
    if not FItems[i].Visible then
      Continue;

    R.Right := R.Left + FItems[I].Width;
    if FItems[I].Enabled then
      UISkin.DrawButtonState(DC, GetActionState(I), R, FItems[I].Fade);
    if LoadActionIcon(I, cIcon) then
    begin
      iOpacity := 255;
      /// 处理不可用状态，图标颜色变暗。
      /// 简易处理，增加绘制透明度。
      if not FItems[I].Enabled then
        iOpacity := 100;

      UISkin.DrawIcon(DC, R, cIcon, iOpacity);
    end;
    OffsetRect(R, R.Right - R.Left, 0);
  end;
  cIcon.free;
end;

procedure TmyCustomToolbar.SetAutoWidth(const Value: Boolean);
begin
  if FAutoWidth <> Value then
  begin
    FAutoWidth := Value;
    UpdateSize;
  end;
end;

procedure TmyCustomToolbar.SetHotIndex(const Value: Integer);
begin
  if FHotIndex <> Value then
  begin
    FHotIndex := Value;
    Invalidate;

    if not(csDestroying in ComponentState) and HandleAllocated then
      SetTimer(Handle, TIMID_FADE, 90, nil);
  end;
end;

procedure TmyCustomToolbar.UpdateFade;

  function GetShowAlpha(v: byte): byte; inline;
  begin
    if v = 0 then           Result := 180
    else if v <= 180 then   Result := 220
    else                    Result := 255;
  end;

  function GetFadeAlpha(v: byte): byte; inline;
  begin
    if v >= 255 then        Result := 230
    else if v >= 230 then   Result := 180
    else if v >= 180 then   Result := 100
    else if v >= 100 then   Result := 50
    else if v >= 50 then    Result := 10
    else                    Result := 0;
  end;

var
  I: Integer;
  bHas: boolean;
begin
  bHas := False;
  for I := 0 to FCount - 1 do
    if FItems[I].Visible and FItems[I].Enabled then
    begin
      if FHotIndex = I then
        FItems[I].Fade := GetShowAlpha(FItems[I].Fade)
      else if FItems[I].Fade > 0 then
        FItems[I].Fade := GetFadeAlpha(FItems[I].Fade);
      bHas := bHas or (FItems[I].Fade > 0);
    end;
  Invalidate;
  if not bHas and HandleAllocated then
    KillTimer(Handle, TIMID_FADE);
end;

procedure TmyCustomToolbar.UpdateSize;
var
  R: TRect;
begin
  if FAutoWidth then
  begin
    R := CalcSize;
    SetBounds(Left, Top, R.Width, Height);
  end
  else
    Invalidate;
end;

procedure TmyCustomToolbar.WMEraseBkgnd(var message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TmyCustomToolbar.WMMouseLeave(var message: TMessage);
begin
  HotIndex := -1;
end;

procedure TmyCustomToolbar.WMMouseMove(var message: TWMMouseMove);
var
  iSave: Integer;
begin
  iSave := FHotIndex;
  HotIndex := HitTest(message.XPos, message.YPos);
  if (iSave <> FHotIndex) and (FHotIndex >= 0) and  (FPressedIndex = -1) then
    Application.ActivateHint(message.Pos);
end;

procedure TmyCustomToolbar.WMPaint(var message: TWMPaint);
var
  DC, hPaintDC: HDC;
  cBuffer: TBitmap;
  PS: TPaintStruct;
  R: TRect;
  w, h: Integer;
begin
  ///
  /// 绘制客户区域
  ///
  R := GetClientRect;
  w := R.Width;
  h := R.Height;

  DC := Message.DC;
  hPaintDC := DC;
  if DC = 0 then
    hPaintDC := BeginPaint(Handle, PS);

  cBuffer := TBitmap.Create;
  try
    cBuffer.SetSize(w, h);
    PaintBackground(cBuffer.Canvas.Handle);
    PaintWindow(cBuffer.Canvas.Handle);
    BitBlt(hPaintDC, 0, 0, w, h, cBuffer.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    cBuffer.free;
  end;

  if DC = 0 then
    EndPaint(Handle, PS);
end;

procedure TmyCustomToolbar.WMTimer(var message: TWMTimer);
begin
  if message.TimerID = TIMID_FADE then
    UpdateFade;
end;

class procedure UISkin.DrawButtonState(DC: HDC; AState: TSkinIndicator; const
    R: TRect; const AOpacity: Byte);
const
  SKINCOLOR_BTNHOT      = $00F2D5C2;  // Hot 激活状态
  SKINCOLOR_BTNPRESSED  = $00E3BDA3;  // 按下状态


  function GetColor(s: TSkinIndicator): Cardinal; inline;
  begin
    case s of
      siHover         : Result := SKINCOLOR_BTNHOT;
      siPressed       : Result := SKINCOLOR_BTNPRESSED;
      siSelected      : Result := SKINCOLOR_BTNPRESSED;
      siHoverSelected : Result := SKINCOLOR_BTNHOT;
    else                Result := SKINCOLOR_BTNHOT;
    end;
  end;

  procedure DrawStyle(DC: HDC; const R: TRect; AColor: Cardinal); inline;
  var
    hB: HBRUSH;
  begin
    hB := CreateSolidBrush(AColor);
    FillRect(DC, R, hB);
    DeleteObject(hB);
  end;

var
  cBmp: TBitmap;
begin
  if AOpacity = 255 then
      DrawStyle(DC, R, GetColor(AState))
  else if AOpacity > 0 then
  begin
    cBmp := TBitmap.Create;
    cBmp.SetSize(r.Width, r.Height);
    DrawStyle(cBmp.Canvas.Handle, Rect(0, 0, r.Width, r.Height), GetColor(AState));
    DrawTransparentBitmap(cBmp, 0, 0, DC, r.Left, r.Top, r.Width, r.Height, AOpacity);
    cBmp.Free;
  end;
end;

class procedure UISkin.DrawIcon(DC: HDC; R: TRect; ASrc: TBitmap; const
    Opacity: Byte = 255);
var
  iXOff: Integer;
  iYOff: Integer;
begin
  ///
  ///  绘制图标
  ///    绘制图标是会作居中处理
  iXOff := r.Left + (R.Right - R.Left - ASrc.Width) div 2;
  iYOff := r.Top + (r.Bottom - r.Top - ASrc.Height) div 2;
  DrawTransparentBitmap(ASrc, 0, 0, DC, iXOff, iYOff, ASrc.Width, ASrc.Height, Opacity);
end;

class procedure UISkin.DrawTransparentBitmap(Source: TBitmap; sx, sy: Integer;
    Destination: HDC; const dX, dY: Integer; w, h: Integer; const Opacity:
    Byte);
var
  BlendFunc: TBlendFunction;
begin
  BlendFunc.BlendOp := AC_SRC_OVER;
  BlendFunc.BlendFlags := 0;
  BlendFunc.SourceConstantAlpha := Opacity;

  if Source.PixelFormat = pf32bit then
    BlendFunc.AlphaFormat := AC_SRC_ALPHA
  else
    BlendFunc.AlphaFormat := 0;

  AlphaBlend(Destination, dX, dY, w, h, Source.Canvas.Handle, sx, sy, w, h, BlendFunc);
end;

end.
