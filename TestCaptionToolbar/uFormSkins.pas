unit uFormSkins;

interface

uses
  Classes, windows, Controls, Graphics, Forms, messages, pngimage, Types, ImgList, ActnList;

const
  WM_NCUAHDRAWCAPTION = $00AE;

  CKM_ADD             = WM_USER + 1;  // 增加标题区域位置

  HTCUSTOM = 100; //HTHELP + 1;              /// 自定义区域ID
  HTCAPTIONTOOLBAR = HTCUSTOM + 1;    /// 标题工具区域

type
  TskForm = class;

  TFormButtonKind = (fbkMin, fbkMax, fbkRestore, fbkClose, fbkHelp);
  TSkinIndicator = (siInactive, siHover, siPressed, siSelected, siHoverSelected);

  TFormCaptionPlugin = class
  private
    FOffset: TPoint;  // 实际标题区域所在的偏移位置
    FBorder: TRect;
    FOwner: TskForm;
    FVisible: Boolean;

  protected
    procedure Paint(DC: HDC); virtual; abstract;
    function  CalcSize: TRect; virtual; abstract;
    function  ScreenToClient(x, y: Integer): TPoint;

    function  HandleMessage(var Message: TMessage): Boolean; virtual;

    procedure HitWindowTest(P: TPoint); virtual;
    procedure MouseMove(p: TPoint); virtual;
    procedure MouseDown(Button: TMouseButton; p: TPoint); virtual;
    procedure MouseUp(Button: TMouseButton; p: TPoint); virtual;
    procedure MouseLeave; virtual;

    procedure Invalidate;
    procedure Update;
  public
    constructor Create(AOwner: TskForm); virtual;

    property Border: TRect read FBorder;
    property Visible: Boolean read FVisible;
  end;

  TcpToolButton = record
    Action: TBasicAction;
    ImageIndex: Integer;    // 考虑到标题功能图标和实际工具栏功能使用不同图标情况，分开图标索引
    Width: Word;            // 实际占用宽度，考虑后续加不同的按钮样式使用
    Fade: Word              // 褪色量 0 - 255
  end;

  TcpToolbar = class(TFormCaptionPlugin)
  private
    FItems: array of TcpToolButton;
    FCount: Integer;
    FHotIndex: Integer;

    // 考虑标题栏比较特殊，背景使用的是纯属情况。图标需要做的更符合纯属需求。
    FImages: TCustomImageList;
    FPressedIndex: Integer;

    procedure ExecAction(Index: Integer);
    procedure PopConfigMenu;
    function  HitTest(P: TPoint): integer;
    function  LoadActionIcon(Idx: Integer; AImg: TBitmap):Boolean;
    procedure SetImages(const Value: TCustomImageList);
  protected
    // 绘制按钮样式
    procedure Paint(DC: HDC); override;
    // 计算实际占用尺寸
    function  CalcSize: TRect; override;

    procedure HitWindowTest(P: TPoint); override;
    procedure MouseMove(p: TPoint); override;
    procedure MouseDown(Button: TMouseButton; p: TPoint); override;
    procedure MouseUp(Button: TMouseButton; p: TPoint); override;
    procedure MouseLeave; override;

  public
    constructor Create(AOwner: TskForm); override;

    procedure Add(Action: TBasicAction; AImageIndex: Integer = -1);
    procedure Delete(Index: Integer);
    function  IndexOf(Action: TBasicAction): Integer;

    property Images: TCustomImageList read FImages write SetImages;
  end;


  TskForm = class
  private
    FCallDefaultProc: Boolean;
    FChangeSizeCalled: Boolean;
    FControl: TWinControl;
    FHandled: Boolean;

    FRegion: HRGN;
    FLeft: integer;
    FTop: integer;
    FWidth: integer;
    FHeight: integer;

    /// 窗体图标
    FIcon: TIcon;
    FIconHandle: HICON;

    // 鼠标位置状态，只处理监控的位置，其他有交由系统处理
    FPressedHit: Integer;     // 实际按下的位置
    FHotHit: integer;         // 记录上次的测试位置

    FToolbar: TcpToolbar;

    function GetHandle: HWND; inline;
    function GetForm: TCustomForm; inline;
    function GetFrameSize: TRect;
    function GetCaptionRect(AMaxed: Boolean): TRect; inline;
    function GetCaption: string;
    function GetIcon: TIcon;
    function GetIconFast: TIcon;

    procedure ChangeSize;
    function  NormalizePoint(P: TPoint): TPoint;
    function  HitTest(P: TPoint):integer;
    procedure Maximize;
    procedure Minimize;

    // 第一组 实现绘制基础
    procedure WMNCPaint(var message: TWMNCPaint); message WM_NCPAINT;
    procedure WMNCActivate(var message: TMessage); message WM_NCACTIVATE;
    procedure WMNCLButtonDown(var message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCUAHDrawCaption(var message: TMessage); message WM_NCUAHDRAWCAPTION;

    // 第二组 控制窗体样式
    procedure WMNCCalcSize(var message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMWindowPosChanging(var message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;

    // 第三组 绘制背景和内部控件
    procedure WMEraseBkgnd(var message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var message: TWMPaint); message WM_PAINT;

    // 第四组 控制按钮状态
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonUp(var Message: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure WMNCMouseMove(var Message: TWMNCMouseMove); message WM_NCMOUSEMOVE;
    procedure WMSetText(var Message: TMessage); message WM_SETTEXT;

    procedure WndProc(var message: TMessage);

    procedure CallDefaultProc(var message: TMessage);
  protected
    property  Handle: HWND read GetHandle;
    procedure InvalidateNC;
    procedure PaintNC(DC: HDC);
    procedure PaintBackground(DC: HDC);
    procedure Paint(DC: HDC);

  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;

    function DoHandleMessage(var message: TMessage): Boolean;

    property Toolbar: TcpToolbar read FToolbar;
    property Handled: Boolean read FHandled write FHandled;
    property Control: TWinControl read FControl;
    property Form: TCustomForm read GetForm;
  end;


implementation

const
  SPALCE_CAPTIONAREA = 3;

{$R MySkin.RES}

type
  TacWinControl = class(TWinControl);

  Res = class
    class procedure LoadGraphic(const AName: string; AGraphic: TGraphic);
    class procedure LoadBitmap(const AName: string; AGraphic: TBitmap);
  end;

  TResArea = record
    x: Integer;
    y: Integer;
    w: Integer;
    h: Integer;
  end;

  TSkinToolbarElement = (steSplitter, stePopdown);

  SkinData = class
  private
  class var
    FData: TBitmap;

  public
    class constructor Create;
    class destructor Destroy;

    class procedure DrawButtonBackground(DC: HDC; AState: TSkinIndicator; const R: TRect; const Opacity: Byte = 255); static;
    class procedure DrawButton(DC: HDC; AKind: TFormButtonKind; AState: TSkinIndicator; const R: TRect); static;
    class procedure DrawElement(DC: HDC; AItem: TSkinToolbarElement; const R: TRect);
    class procedure DrawIcon(DC: HDC; R: TRect; ASrc: TBitmap);
  end;

const
  SKINCOLOR_BAKCGROUND  = $00BF7B18;  // 背景色
  SKINCOLOR_BTNHOT      = $00F2D5C2;  // Hot 激活状态
  SKINCOLOR_BTNPRESSED  = $00E3BDA3;  // 按下状态
  SIZE_SYSBTN: TSize    = (cx: 29; cy: 18);
  SIZE_FRAME: TRect     = (Left: 4; Top: 29; Right: 5; Bottom: 5); // 窗体边框的尺寸
  SPACE_AREA            = 3;          // 功能区域之间间隔
  SIZE_RESICON          = 16;         // 资源中图标默认尺寸
  SIZE_HEIGHTTOOLBAR    = 16;

  RES_CAPTIONTOOLBAR: TResArea = (x: 0; y: 16; w: 9; h: 16);


function BuildRect(L, T, W, H: Integer): TRect; inline;
begin
  Result := Rect(L, T, L + W, T + H);
end;

procedure DrawTransparentBitmap(Source: TBitmap; sx, sy: Integer; Destination: HDC;
  const dX, dY: Integer;  w, h: Integer; const Opacity: Byte = 255); overload;
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


procedure TskForm.CallDefaultProc(var message: TMessage);
begin
  if FCallDefaultProc then
    FControl.WindowProc(message)
  else
  begin
    FCallDefaultProc := True;
    FControl.WindowProc(message);
    FCallDefaultProc := False;
  end;
end;

procedure TskForm.ChangeSize;
var
  hTmp: HRGN;
begin
  /// 设置窗体外框样式
  FChangeSizeCalled := True;
  try
    hTmp := FRegion;
    try
      /// 创建矩形外框，3的倒角
      FRegion := CreateRoundRectRgn(0, 0, FWidth, FHeight, 3, 3);
      SetWindowRgn(Handle, FRegion, True);
    finally
      if hTmp <> 0 then
        DeleteObject(hTmp);
    end;
  finally
    FChangeSizeCalled := False;
  end;
end;

function TskForm.NormalizePoint(P: TPoint): TPoint;
var
  rWindowPos, rClientPos: TPoint;
begin
  rWindowPos := Point(FLeft, FTop);
  rClientPos := Point(0, 0);
  ClientToScreen(Handle, rClientPos);
  Result := P;
  ScreenToClient(Handle, Result);
  Inc(Result.X, rClientPos.X - rWindowPos.X);
  Inc(Result.Y, rClientPos.Y - rWindowPos.Y);
end;

function TskForm.HitTest(P: TPoint):integer;
var
  bMaxed: Boolean;
  r: TRect;
  rCaptionRect: TRect;
  rFrame: TRect;
begin
  Result := HTNOWHERE;

  ///
  /// 检测位置
  ///
  rFrame := GetFrameSize;
  if p.Y > rFrame.Top then
    Exit;

  ///
  ///  只关心窗体按钮区域
  ///
  bMaxed := IsZoomed(Handle);
  rCaptionRect := GetCaptionRect(bMaxed);
  if PtInRect(rCaptionRect, p) then
  begin
    r.Right := rCaptionRect.Right - 1;
    r.Top := 0;
    if bMaxed then
      r.Top := rCaptionRect.Top;
    r.Top := r.Top + (rFrame.Top - r.Top - SIZE_SYSBTN.cy) div 2;
    r.Left := r.Right - SIZE_SYSBTN.cx;
    r.Bottom := r.Top + SIZE_SYSBTN.cy;

    ///
    /// 实际绘制的按钮就三个，其他没处理
    ///
    if (P.Y >= r.Top) and (p.Y <= r.Bottom) and (p.X <= r.Right) then
    begin
      if (P.X >= r.Left) then
        Result := HTCLOSE
      else if p.X >= (r.Left - SIZE_SYSBTN.cx) then
        Result := HTMAXBUTTON
      else if p.X >= (r.Left - SIZE_SYSBTN.cx * 2) then
        Result := HTMINBUTTON;
    end;

    ///
    ///  标题工具区域
    ///    需要前面扣除窗体图标区域
    if (Result = HTNOWHERE) and (FToolbar.Visible) then
    begin
      r.Left := rCaptionRect.Left + 2 + GetSystemMetrics(SM_CXSMICON) + SPALCE_CAPTIONAREA;
      R.Top := rCaptionRect.Top + (rCaptionRect.Height - FToolbar.Border.Height) div 2;
      R.Right := R.Left + FToolbar.Border.Width;
      R.Bottom := R.Top + FToolbar.Border.Height;

      if FToolbar.FOffset.X = -1 then
        FToolbar.FOffset := r.TopLeft;

      if PtInRect(r, p) then
        Result := HTCAPTIONTOOLBAR;
    end;
  end;
end;

constructor TskForm.Create(AOwner: TWinControl);
begin
  FControl := AOwner;
  FRegion := 0;
  FChangeSizeCalled := False;
  FCallDefaultProc := False;

  FWidth := FControl.Width;
  FHeight := FControl.Height;
  FIcon := nil;
  FIconHandle := 0;

  FToolbar := TcpToolbar.Create(Self);
end;

destructor TskForm.Destroy;
begin
  FToolbar.Free;

  FIconHandle := 0;
  if FIcon <> nil then      FIcon.Free;
  if FRegion <> 0 then      DeleteObject(FRegion);
  inherited;
end;

function TskForm.DoHandleMessage(var message: TMessage): Boolean;
begin
  Result := False;
  if not FCallDefaultProc then
  begin
    FHandled := False;
    WndProc(message);
    Result := Handled;
  end;
end;

function TskForm.GetFrameSize: TRect;
begin
  Result := SIZE_FRAME;
end;

function TskForm.GetCaptionRect(AMaxed: Boolean): TRect;
var
  rFrame: TRect;
begin
  rFrame := GetFrameSize;
  // 最大化状态简易处理
  if AMaxed then
    Result := Rect(8, 8, FWidth - 9 , rFrame.Top)
  else
    Result := Rect(rFrame.Left, 3, FWidth - rFrame.right, rFrame.Top);
end;

function TskForm.GetCaption: string;
var
  Buffer: array [0..255] of Char;
  iLen: integer;
begin
  if Handle <> 0 then
  begin
    iLen := GetWindowText(Handle, Buffer, Length(Buffer));
    SetString(Result, Buffer, iLen);
  end
  else
    Result := '';
end;

function TskForm.GetForm: TCustomForm;
begin
  Result := TCustomForm(Control);
end;

function TskForm.GetHandle: HWND;
begin
  if FControl.HandleAllocated then
    Result := FControl.Handle
  else
    Result := 0;
end;

function TskForm.GetIcon: TIcon;
var
  IconX, IconY: integer;
  TmpHandle: THandle;
  Info: TWndClassEx;
  Buffer: array [0 .. 255] of Char;
begin
  ///
  /// 获取当前form的图标
  /// 这个图标和App的图标是不同的
  ///
  TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_SMALL, 0));
  if TmpHandle = 0 then
    TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_BIG, 0));

  if TmpHandle = 0 then
  begin
    { Get instance }
    GetClassName(Handle, @Buffer, SizeOf(Buffer));
    FillChar(Info, SizeOf(Info), 0);
    Info.cbSize := SizeOf(Info);

    if GetClassInfoEx(GetWindowLong(Handle, GWL_HINSTANCE), @Buffer, Info) then
    begin
      TmpHandle := Info.hIconSm;
      if TmpHandle = 0 then
        TmpHandle := Info.HICON;
    end
  end;

  if FIcon = nil then
    FIcon := TIcon.Create;

  if TmpHandle <> 0 then
  begin
    IconX := GetSystemMetrics(SM_CXSMICON);
    if IconX = 0 then
      IconX := GetSystemMetrics(SM_CXSIZE);
    IconY := GetSystemMetrics(SM_CYSMICON);
    if IconY = 0 then
      IconY := GetSystemMetrics(SM_CYSIZE);
    FIcon.Handle := CopyImage(TmpHandle, IMAGE_ICON, IconX, IconY, 0);
    FIconHandle := TmpHandle;
  end;

  Result := FIcon;
end;

function TskForm.GetIconFast: TIcon;
begin
  if (FIcon = nil) or (FIconHandle = 0) then
    Result := GetIcon
  else
    Result := FIcon;
end;

procedure TskForm.InvalidateNC;
begin
  if FControl.HandleAllocated then
    SendMessage(Handle, WM_NCPAINT, 1, 0);
end;

procedure TskForm.Maximize;
begin
  if Handle <> 0 then
  begin
    FPressedHit := 0;
    FHotHit := 0;
    if IsZoomed(Handle) then
      SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      SendMessage(Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
  end;
end;

procedure TskForm.Minimize;
begin
  if Handle <> 0 then
  begin
    FPressedHit := 0;
    FHotHit := 0;
    if IsIconic(Handle) then
      SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
    else
      SendMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
   end;
end;

procedure TskForm.PaintNC(DC: HDC);
const
  HITVALUES: array [TFormButtonKind] of integer = (HTMINBUTTON, HTMAXBUTTON, HTMAXBUTTON, HTCLOSE, HTHELP);

  function GetBtnState(AKind: TFormButtonKind): TSkinIndicator;
  begin
    if (FPressedHit = FHotHit) and (FPressedHit = HITVALUES[AKind]) then
      Result := siPressed
    else if FHotHit = HITVALUES[AKind] then
      Result := siHover
    else
      Result := siInactive;
  end;

var
  bClipRegion: boolean;
  hB: HBRUSH;
  rFrame: TRect;
  rButton: TRect;
  SaveIndex: integer;
  bMaxed: Boolean;
  ClipRegion: HRGN;
  CurrentIdx: Integer;
  rCaptionRect : TRect;
  sData: string;
  Flag: Cardinal;
  iLeftOff: Integer;
  iTopOff: Integer;
  SaveColor: cardinal;
begin
  SaveIndex := SaveDC(DC);
  try
    bMaxed := IsZoomed(Handle);

    // 扣除客户区域
    rFrame := GetFrameSize;
    ExcludeClipRect(DC, rFrame.Left, rFrame.Top, FWidth - rFrame.Right, FHeight - rFrame.Bottom);

    ///
    ///  标题区域
    ///
    rCaptionRect := GetCaptionRect(bMaxed);

    // 填充整个窗体背景
    hB := CreateSolidBrush(SKINCOLOR_BAKCGROUND);
    FillRect(DC, Rect(0, 0, FWidth, FHeight), hB);
    DeleteObject(hB);

    ///
    /// 绘制窗体图标
    rButton := BuildRect(rCaptionRect.Left + 2, rCaptionRect.Top, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    rButton.Top := rButton.Top + (rFrame.Top - rButton.Bottom) div 2;
    DrawIconEx(DC, rButton.Left, rButton.Top, GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);
    rCaptionRect.Left := rButton.Right + SPALCE_CAPTIONAREA; //

    ///
    /// 绘制窗体按钮区域
    rButton.Right := rCaptionRect.Right - 1;
    rButton.Top := 0;
    if bMaxed then
      rButton.Top := rCaptionRect.Top;
    rButton.Top := rButton.Top + (rFrame.Top - rButton.Top - SIZE_SYSBTN.cy) div 2;
    rButton.Left := rButton.Right - SIZE_SYSBTN.cx;
    rButton.Bottom := rButton.Top + SIZE_SYSBTN.cy;
    SkinData.DrawButton(Dc, fbkClose, GetBtnState(fbkClose), rButton);

    OffsetRect(rButton, - SIZE_SYSBTN.cx, 0);
    if bMaxed then
      SkinData.DrawButton(Dc, fbkRestore, GetBtnState(fbkRestore), rButton)
    else
      SkinData.DrawButton(Dc, fbkMax, GetBtnState(fbkMax), rButton);

    OffsetRect(rButton, - SIZE_SYSBTN.cx, 0);
    SkinData.DrawButton(Dc, fbkMin, GetBtnState(fbkMin), rButton);
    rCaptionRect.Right := rButton.Left - SPALCE_CAPTIONAREA; // 后部空出

    ///
    /// 绘制工具条
    if FToolbar.Visible and (rCaptionRect.Right > rCaptionRect.Left) then
    begin
      /// 防止出现绘制出多余区域，当区域不够时需要进行剪切。
      ///  如： 窗体缩小时
      CurrentIdx := 0;
      bClipRegion := rCaptionRect.Width < FToolbar.Border.Width;
      if bClipRegion then
      begin
        ClipRegion := CreateRectRgnIndirect(rCaptionRect);
        CurrentIdx := SelectClipRgn(DC, ClipRegion);
        DeleteObject(ClipRegion);
      end;

      iLeftOff := rCaptionRect.Left;
      iTopOff := rCaptionRect.Top + (rCaptionRect.Height - FToolbar.Border.Height) div 2;
      MoveWindowOrg(DC, iLeftOff, iTopOff);
      FToolbar.Paint(DC);
      MoveWindowOrg(DC, -iLeftOff, -iTopOff);

      if bClipRegion then
        SelectClipRgn(DC, CurrentIdx);

      /// 扣除工具条区域
      rCaptionRect.Left := rCaptionRect.Left + FToolbar.Border.Width + SPALCE_CAPTIONAREA;
    end;

    ///
    /// 绘制Caption
    if rCaptionRect.Right > rCaptionRect.Left then
    begin
      sData :=  GetCaption;
      SetBkMode(DC, TRANSPARENT);
      SaveColor := SetTextColor(DC, $00FFFFFF);

      Flag := DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
      DrawTextEx(DC, PChar(sData), Length(sData), rCaptionRect, Flag, nil);
      SetTextColor(DC, SaveColor);
    end;
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TskForm.PaintBackground(DC: HDC);
var
  hB: HBRUSH;
  R: TRect;
begin
  GetClientRect(Handle, R);
  hB := CreateSolidBrush($00F0F0F0);
  FillRect(DC, R, hB);
  DeleteObject(hB);
end;

procedure TskForm.Paint(DC: HDC);
begin
  // PaintBackground(DC);
  // TODO -cMM: TskForm.Paint default body inserted
end;

procedure TskForm.WMEraseBkgnd(var message: TWMEraseBkgnd);
var
  DC: HDC;
  SaveIndex: integer;
begin
  DC := Message.DC;
  if DC <> 0 then
  begin
    SaveIndex := SaveDC(DC);
    PaintBackground(DC);
    RestoreDC(DC, SaveIndex);
  end;

  Handled := True;
  Message.Result := 1;
end;

procedure TskForm.WMNCActivate(var message: TMessage);
begin
  // FFormActive := Message.WParam > 0;
  Message.Result := 1;
  InvalidateNC;
  Handled := True;
end;

procedure TskForm.WMNCCalcSize(var message: TWMNCCalcSize);
var
  R: TRect;
begin
  // 改变边框尺寸
  R := GetFrameSize;
  with TWMNCCalcSize(Message).CalcSize_Params^.rgrc[0] do
  begin
    Inc(Left, R.Left);
    Inc(Top, R.Top);
    Dec(Right, R.Right);
    Dec(Bottom, R.Bottom);
  end;
  Message.Result := 0;
  Handled := True;
end;

procedure TskForm.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  iHit: integer;
begin
  // 需要把位置转换到实际窗口位置
  P := NormalizePoint(Point(Message.XPos, Message.YPos));

  // 获取 位置
  iHit := HitTest(p);
  if FHotHit > HTNOWHERE then
  begin
    Message.Result := iHit;
    Handled := True;
  end;

  if iHit <> FHotHit then
  begin
    if FHotHit = HTCAPTIONTOOLBAR then
      FToolbar.MouseLeave;

    FHotHit := iHit;
    InvalidateNC;
  end;

end;

procedure TskForm.WMWindowPosChanging(var message: TWMWindowPosChanging);
var
  bChanged: Boolean;
begin
  CallDefaultProc(TMessage(Message));

  Handled := True;
  bChanged := False;

  /// 防止嵌套
  if FChangeSizeCalled then
    Exit;

  if (Message.WindowPos^.flags and SWP_NOSIZE = 0) or (Message.WindowPos^.flags and SWP_NOMOVE = 0) then
  begin
    if (Message.WindowPos^.flags and SWP_NOMOVE = 0) then
    begin
      FLeft := Message.WindowPos^.x;
      FTop := Message.WindowPos^.y;
    end;
    if (Message.WindowPos^.flags and SWP_NOSIZE = 0) then
    begin
      bChanged := ((Message.WindowPos^.cx <> FWidth) or (Message.WindowPos^.cy <> FHeight)) and
        (Message.WindowPos^.flags and SWP_NOSIZE = 0);
      FWidth := Message.WindowPos^.cx;
      FHeight := Message.WindowPos^.cy;
    end;
  end;

  if (Message.WindowPos^.flags and SWP_FRAMECHANGED <> 0) then
    bChanged := True;

  if bChanged then
  begin
    ChangeSize;
    InvalidateNC;
  end;
end;

procedure TskForm.WMNCLButtonDown(var message: TWMNCLButtonDown);
var
  iHit: integer;
begin
  iHit := HTNOWHERE;
  if (Message.HitTest = HTCLOSE) or (Message.HitTest = HTMAXBUTTON) or (Message.HitTest = HTMINBUTTON) or
    (Message.HitTest = HTHELP) or (Message.HitTest > HTCUSTOM) then
    iHit := Message.HitTest;


  /// 只处理系统按钮和自定义区域
  if iHit <> HTNOWHERE then
  begin
    if iHit <> FPressedHit then
    begin
      FPressedHit := iHit;
      if FPressedHit = HTCAPTIONTOOLBAR then
        FToolbar.HandleMessage(TMessage(message));
      InvalidateNC;
    end;

    Message.Result := 0;
    Message.Msg := WM_NULL;
    Handled := True;
  end;
end;

procedure TskForm.WMNCLButtonUp(var Message: TWMNCLButtonUp);
var
  iWasHit: Integer;
begin
  iWasHit := FPressedHit;
  if iWasHit <> HTNOWHERE then
  begin
    FPressedHit := HTNOWHERE;
    //InvalidateNC;

    if iWasHit = FHotHit then
    begin
      case Message.HitTest of
        HTCLOSE           : SendMessage(Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
        HTMAXBUTTON       : Maximize;
        HTMINBUTTON       : Minimize;
        HTHELP            : SendMessage(Handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0);

        HTCAPTIONTOOLBAR  : FToolbar.HandleMessage(TMessage(Message));
      end;

      Message.Result := 0;
      Message.Msg := WM_NULL;
      Handled := True;
    end;
  end;
end;

procedure TskForm.WMNCMouseMove(var Message: TWMNCMouseMove);
begin
  if Message.HitTest = HTCAPTIONTOOLBAR then
  begin
    FToolbar.HandleMessage(TMessage(Message));
    Handled := True;
  end
  else
  begin
    if (FPressedHit <> HTNOWHERE) and (FPressedHit <> Message.HitTest) then
      FPressedHit := HTNOWHERE;
  end;
end;

procedure TskForm.WMSetText(var Message: TMessage);
begin
  CallDefaultProc(Message);
  InvalidateNC;
  Handled := true;
end;

procedure TskForm.WMNCPaint(var message: TWMNCPaint);
var
  DC: HDC;
begin
  DC := GetWindowDC(Control.Handle);
  PaintNC(DC);
  ReleaseDC(Handle, DC);
  Handled := True;
end;

procedure TskForm.WMNCUAHDrawCaption(var message: TMessage);
begin
  /// 这个消息会在winxp下产生，是内部Bug处理，直接丢弃此消息
  Handled := True;
end;

procedure TskForm.WMPaint(var message: TWMPaint);
var
  DC, hPaintDC: HDC;
  cBuffer: TBitmap;
  PS: TPaintStruct;
begin
  ///
  /// 绘制客户区域
  ///
  DC := Message.DC;

  hPaintDC := DC;
  if DC = 0 then
    hPaintDC := BeginPaint(Handle, PS);

  if DC = 0 then
  begin
    /// 缓冲模式绘制，减少闪烁
    cBuffer := TBitmap.Create;
    try
      cBuffer.SetSize(FWidth, FHeight);
      PaintBackground(cBuffer.Canvas.Handle);
      Paint(cBuffer.Canvas.Handle);
      /// 通知子控件进行绘制
      /// 主要是些图形控件的重绘制（如TShape），否则停靠在Form上的图像控件无法正常显示
      if Control is TWinControl then
        TacWinControl(Control).PaintControls(cBuffer.Canvas.Handle, nil);
      BitBlt(hPaintDC, 0, 0, FWidth, FHeight, cBuffer.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      cBuffer.Free;
    end;
  end
  else
  begin
    Paint(hPaintDC);
    // 通知子控件重绘
    if Control is TWinControl then
      TacWinControl(Control).PaintControls(hPaintDC, nil);
  end;

  if DC = 0 then
    EndPaint(Handle, PS);

  Handled := True;
end;

procedure TskForm.WndProc(var message: TMessage);
begin
  FHandled := False;
  Dispatch(message);
end;

class procedure Res.LoadBitmap(const AName: string; AGraphic: TBitmap);
var
  cPic: TPngImage;
  cBmp: TBitmap;
begin
  cBmp := AGraphic;
  cPic := TPngImage.Create;
  try
    cBmp.PixelFormat := pf32bit;
    cBmp.alphaFormat := afIgnored;
    try
      LoadGraphic(AName, cPic);
      cBmp.SetSize(cPic.Width, cPic.Height);
      cBmp.Canvas.Brush.Color := clBlack;
      cBmp.Canvas.FillRect(Rect(0, 0, cBmp.Width, cBmp.Height));
      cBmp.Canvas.Draw(0, 0, cPic);
    except
      // 不处理空图片
    end;
  finally
    cPic.Free;
  end;
end;

class procedure Res.LoadGraphic(const AName: string; AGraphic: TGraphic);
var
  cStream: TResourceStream;
  h: THandle;
begin
  ///
  /// 加载图片资源
  h := HInstance;
  cStream := TResourceStream.Create(h, AName, RT_RCDATA);
  try
    AGraphic.LoadFromStream(cStream);
  finally
    cStream.Free;
  end;
end;

class constructor SkinData.Create;
begin
  // 加载资源
  FData := TBitmap.Create;
  Res.LoadBitmap('MySkin', FData);
end;

class destructor SkinData.Destroy;
begin
  FData.Free;
end;

class procedure SkinData.DrawButton(DC: HDC; AKind: TFormButtonKind; AState:
    TSkinIndicator; const R: TRect);
var
  rSrcOff: TPoint;
  x, y: integer;
begin
  /// 绘制背景
  DrawButtonBackground(DC, AState, R);

  /// 绘制图标
  rSrcOff := Point(SIZE_RESICON * ord(AKind), 0);
  x := R.Left + (R.Right - R.Left - SIZE_RESICON) div 2;
  y := R.Top + (R.Bottom - R.Top - SIZE_RESICON) div 2;
  DrawTransparentBitmap(FData, rSrcOff.X, rSrcOff.Y, DC, x, y, SIZE_RESICON, SIZE_RESICON);
end;

class procedure SkinData.DrawButtonBackground(DC: HDC; AState: TSkinIndicator; const R: TRect; const Opacity: Byte = 255);
var
  hB: HBRUSH;
  iColor: Cardinal;
begin
  if AState <> siInactive then
  begin
    /// 绘制背景
    case AState of
      siHover         : iColor := SKINCOLOR_BTNHOT;
      siPressed       : iColor := SKINCOLOR_BTNPRESSED;
      siSelected      : iColor := SKINCOLOR_BTNPRESSED;
      siHoverSelected : iColor := SKINCOLOR_BTNHOT;
    else                iColor := SKINCOLOR_BAKCGROUND;
    end;
    hB := CreateSolidBrush(iColor);
    FillRect(DC, R, hB);
    DeleteObject(hB);
  end;
end;

class procedure SkinData.DrawElement(DC: HDC; AItem: TSkinToolbarElement; const R: TRect);
var
  rSrc: TResArea;
  x, y: integer;
begin
  rSrc := RES_CAPTIONTOOLBAR;
  rSrc.x :=  rSrc.x + rSrc.w * (ord(AItem) - ord(Low(TSkinToolbarElement)));

  /// 绘制图标
  x := R.Left + (R.Right - R.Left - rSrc.w) div 2;
  y := R.Top + (R.Bottom - R.Top - rSrc.h) div 2;
  DrawTransparentBitmap(FData, rSrc.x, rSrc.y, DC, x, y, rSrc.w, rSrc.h);
end;

class procedure SkinData.DrawIcon(DC: HDC; R: TRect; ASrc: TBitmap);
var
  iXOff: Integer;
  iYOff: Integer;
begin
  iXOff := r.Left + (R.Right - R.Left - ASrc.Width) div 2;
  iYOff := r.Top + (r.Bottom - r.Top - ASrc.Height) div 2;
  DrawTransparentBitmap(ASrc, 0, 0, DC, iXOff, iYOff, ASrc.Width, ASrc.Height);
end;

{ TcpToolbar }
constructor TcpToolbar.Create(AOwner: TskForm);
begin
  inherited;
  FHotIndex := -1;
  FPressedIndex := -1;
end;

procedure TcpToolbar.Add(Action: TBasicAction; AImageIndex: Integer);
begin
  if FCount >= Length(FItems) then
    SetLength(FItems, FCount + 5);

  ZeroMemory(@FItems[FCount], SizeOf(TcpToolButton));
  FItems[FCount].Action := Action;
  FItems[FCount].ImageIndex := AImageIndex;
  FItems[FCount].Width := 20;
  FItems[FCount].Fade  := 255;

  inc(FCount);

  Update;
end;

function TcpToolbar.CalcSize: TRect;
const
  SIZE_SPLITER = 10;
  SIZE_POPMENU = 10;
  SIZE_BUTTON  = 20;
var
  w, h: Integer;
  I: Integer;
begin
  ///
  ///  占用宽度
  ///     如果考虑比较复杂的按钮样式和显示标题等功能，那么需要计算每个按钮实际占用宽度才能获得。

  w := SIZE_SPLITER * 2 + SIZE_POPMENU;
  for I := 0 to FCount - 1 do
    w := w + FItems[i].Width;
  h := SIZE_BUTTON;
  Result := Rect(0, 0, w, h);
end;

procedure TcpToolbar.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    if Index < (FCount - 1) then
      Move(FItems[Index+1], FItems[Index], sizeof(TcpToolButton) * (FCount - Index - 1));
    dec(FCount);

    Update;
  end;
end;

function TcpToolbar.HitTest(P: TPoint): integer;
var
  iOff: Integer;
  iIdx: integer;
  I: Integer;
begin
  ///
  ///  检测鼠标位置
  ///    鼠标位置的 FCount位 为工具条系统菜单位置。
  iIdx := -1;
  iOff := RES_CAPTIONTOOLBAR.w;
  if p.x > iOff then
  begin
    for I := 0 to FCount - 1 do
    begin
      if p.X < iOff then
        Break;

      iIdx := i;
      inc(iOff, FItems[i].Width);
    end;

    if p.x > iOff then
    begin
      iIdx := -1;
      inc(iOff, RES_CAPTIONTOOLBAR.w);
      if p.x > iOff then
        iIdx := FCount;  // FCount 为系统菜单按钮
    end;
  end;

  Result := iIdx;
end;

procedure TcpToolbar.ExecAction(Index: Integer);
begin
  ///
  /// 执行命令
  ///
  if (Index >= 0) and (Index < FCount) then
    FItems[Index].Action.Execute;

  // FCount位 为系统配置按钮
  if Index = FCount then
    PopConfigMenu;
end;

procedure TcpToolbar.PopConfigMenu;
begin
end;

procedure TcpToolbar.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Invalidate;
end;

function TcpToolbar.IndexOf(Action: TBasicAction): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if FItems[i].Action = Action then
    begin
      Result := i;
      Break;
    end;
end;

procedure TcpToolbar.MouseDown(Button: TMouseButton; p: TPoint);
begin
  if (mbLeft = Button) then
  begin
    FPressedIndex := HitTest(p);
    //Invalidate;
  end;
end;

procedure TcpToolbar.MouseLeave;
begin
  if FHotIndex >= 0 then
  begin
    FHotIndex := -1;
    //Invalidate;
  end;
end;

procedure TcpToolbar.HitWindowTest(P: TPoint);
begin
  FHotIndex := HitTest(P);
end;

procedure TcpToolbar.MouseMove(p: TPoint);
var
  iIdx: Integer;
begin
  iIdx := HitTest(p);
  if iIdx <> FHotIndex then
  begin
    FHotIndex := iIdx;
    Invalidate;
  end;
end;

procedure TcpToolbar.MouseUp(Button: TMouseButton; p: TPoint);
var
  iAction: Integer;
begin
  if (mbLeft = Button) and (FPressedIndex >= 0) and (FHotIndex = FPressedIndex) then
  begin
    iAction := FPressedIndex;
    FPressedIndex := -1;
    Invalidate;

    ExecAction(iAction);
  end;
end;

function TcpToolbar.LoadActionIcon(Idx: Integer; AImg: TBitmap):Boolean;
var
  bHasImg: Boolean;
begin
  /// 获取Action的图标
  AImg.Canvas.Brush.Color := clBlack;
  AImg.Canvas.FillRect(Rect(0,0, AImg.Width, AImg.Height));
  bHasImg := False;
  if (FImages <> nil) and (FItems[Idx].ImageIndex >= 0) then
    bHasImg := FImages.GetBitmap(FItems[Idx].ImageIndex, AImg);
  if not bHasImg and (FItems[Idx].Action is TCustomAction) then
    with TCustomAction(FItems[Idx].Action) do
      if (Images <> nil) and (ImageIndex >= 0) then
        bHasImg := Images.GetBitmap(ImageIndex, AImg);
  Result := bHasImg;
end;

procedure TcpToolbar.Paint(DC: HDC);

  function GetActionState(Idx: Integer): TSkinIndicator;
  begin
    Result := siInactive;
    if (Idx = FPressedIndex) and (FHotIndex = FPressedIndex) then
      Result := siPressed
    else if Idx = FHotIndex then
      Result := siHover;
  end;

var
  cImg: TBitmap;
  r: TRect;
  I: Integer;
begin
  ///
  ///  工具条绘制
  ///

  /// 分割线
  r := Border;
  r.Right := r.Left + RES_CAPTIONTOOLBAR.w;
  SkinData.DrawElement(DC, steSplitter, r);
  OffsetRect(r, r.Right - r.Left, 0);

  /// 绘制Button
  cImg := TBitmap.Create;
  cImg.PixelFormat := pf32bit;
  cImg.alphaFormat := afIgnored;
  for I := 0 to FCount - 1 do
  begin
    r.Right := r.Left + FItems[i].Width;
    SkinData.DrawButtonBackground(DC, GetActionState(i), r, FItems[i].Fade);
    if LoadActionIcon(i, cImg) then
      SkinData.DrawIcon(DC, r, cImg);
    OffsetRect(r, r.Right - r.Left, 0);
  end;
  cImg.free;

  /// 分割条
  r.Right := r.Left + RES_CAPTIONTOOLBAR.w;
  SkinData.DrawElement(DC, steSplitter, r);
  OffsetRect(r, r.Right - r.Left, 0);

  /// 绘制下拉菜单
  r.Right := r.Left + RES_CAPTIONTOOLBAR.w;
  SkinData.DrawElement(DC, stePopdown, r);
end;

constructor TFormCaptionPlugin.Create(AOwner: TskForm);
begin
  FOwner := AOwner;
  FVisible := True;
  FBorder := CalcSize;
  FOffset.X := -1;
end;

function TFormCaptionPlugin.ScreenToClient(x, y: Integer): TPoint;
var
  P: TPoint;
begin
  /// 调整位置
  ///    以 FOffset 为中心位置
  P := FOwner.NormalizePoint(Point(x, Y));
  p.X := p.X - FOffset.X;
  p.Y := p.y - FOffset.Y;

  Result := p;
end;


function TFormCaptionPlugin.HandleMessage(var Message: TMessage): Boolean;
begin
  Result := True;

  case Message.Msg of
    WM_NCMOUSEMOVE    : MouseMove(ScreenToClient(TWMNCMouseMove(Message).XCursor, TWMNCMouseMove(Message).YCursor));
    WM_NCLBUTTONDOWN  : MouseDown(mbLeft, ScreenToClient(TWMNCLButtonDown(Message).XCursor, TWMNCLButtonDown(Message).YCursor));
    WM_NCHITTEST      : HitWindowTest(ScreenToClient(TWMNCHitTest(Message).XPos, TWMNCHitTest(Message).YPos));
    WM_NCLBUTTONUP    : MouseUp(mbLeft, ScreenToClient(TWMNCLButtonUp(Message).XCursor, TWMNCLButtonUp(Message).YCursor));

    else
      Result := False;
  end;
end;

procedure TFormCaptionPlugin.HitWindowTest(P: TPoint);
begin
end;

procedure TFormCaptionPlugin.Invalidate;
begin
  FOwner.InvalidateNC;
end;

procedure TFormCaptionPlugin.MouseDown(Button: TMouseButton; p: TPoint);
begin
end;

procedure TFormCaptionPlugin.MouseLeave;
begin
end;

procedure TFormCaptionPlugin.MouseMove(p: TPoint);
begin
end;

procedure TFormCaptionPlugin.MouseUp(Button: TMouseButton; p: TPoint);
begin
end;

procedure TFormCaptionPlugin.Update;
begin
  FBorder := CalcSize;
  Invalidate;
end;

end.
