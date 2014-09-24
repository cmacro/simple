unit uUISkins;

{$I UI.inc}

interface

uses
  Classes, Winapi.Windows, Types, Graphics, GraphUtil, UITypes, Forms, Math,
  {$ifdef USE_ZLIB}, ZLib{$endif}
  GDIPOBJ, GDIPAPI,

  uUITypes;

{.$define HeaderGradient}


type
  TPathPoints = array [0..29] of TGPPoint;

  TSkinElements = (seVertScroll, seHorzScroll);
  TSkinElemenState = (sesNormal, sesHot, sesPressed, sesDisabled);

  TSkinElementData = record
    case Integer of
      0:  ( X: integer; Y: integer; W: integer; H: integer);
      1:  ( Offset: TPoint; Size: TSize);
  end;

  TResourceAreas = (raCheckbox,           // checkbox 的资源区域
                    raTreeScrollbar,      // TreeView 的滚动条资源  ELEDATA: TSkinElementData = (X: 190; Y: 182; W: 13; H: 11);
                    raFormButton,         // 窗体按钮的区域
                    raFormToolbar         // 窗体Toolbar区域
                    );

  TSkinToolbarElement = (steSplitter, stePopdown);


  //1 元素组位置, 与下面具有组类型的相同，不要改变顺序
  TElementGroupPlace = (egpStart, egpMiddle, egpEnd);
  TSkinTreeView = (stvHeader, stvCellSelected);
  TSkinDataView = (sdvHeader);

  TSkinFrame = (sfNone, sfFlat); //, sfCellFocused, sfDisableFocused

  TRangeSide = (rsdLeft, rsdTop, rsdRight, rsdBottom);
  TRangeSides = set of TRangeSide;
  TRangeHandle = set of (rhLeftTop, rhRightTop, rhLeftBottom, rhRightBottom);
  TSkinRangeFrame = (srfSelectActiveFocused, srfSelectInactiveFocused);

  TSkinIndicator = (
    siInactive, siHover, siPressed, siSelected, siHoverSelected
    );

  TFormButtonKind = (fbkMin, fbkMax, fbkRestore, fbkClose, fbkHelp);

  TSkinHeader = (shNone, shBackground,
    shStartInactive, shMiddleInactive, shEndInactive,
    shStartHover, shMiddleHover, shEndHover,
    shStartPressed, shMiddlePressed, shEndPressed,
    shStartSelected, shMiddleSelected, shEndSelected,
    shStartHoverSelected, shMiddleHoverSelected, shEndHoverSelected,
    shBtnCloseInactive, shBtnCloseHover, shBtnClosePressed,
    shBtnFilterNormal, shBtnFilterHot, shBtnFilterPressed,
    shBtnActiveFilterNormal, shBtnActiveFilterHot, shBtnActiveFilterPressed,
    shHideColumnPrev);

  TSkinButton = (sbInactive, sbDisabled, sbHover, sbPressed, sbSelected, sbHoverSelected,

    //1 省略号按钮
    sbEllipsisNormal,
    sbEllipsisHot,
    sbEllipsisPressed,

    //1 radio
    sbRadioUnRadioedNormal,
    sbRadioUnRadioedHot,
    sbRadioUnRadioedPressed,
    sbRadioUnRadioedDisabled,

    sbRadioRadioedNormal,
    sbRadioRadioedHot,
    sbRadioRadioedPressed,
    sbRadioRadioedDisabled,

    //1 Checkbox
    sbCheckUncheckedNormal,
    sbCheckUncheckedHot,
    sbCheckUncheckedPressed,
    sbCheckUncheckedDisabled,
    //1 check
    sbCheckCheckedNormal,
    sbCheckCheckedHot,
    sbCheckCheckedPressed,
    sbCheckCheckedDisabled,
    //1 Check mix
    sbCheckMixedNormal,
    sbCheckMixedHot,
    sbCheckMixedPressed,
    sbCheckMixedDisabled,

    sbDropButtonNormal,
    sbDropButtonHot,
    sbDropButtonPressed,
    sbDropButtonDisabled
  );


  TSkinScrollType = (
    sstVert,
    sstHorz
  );

  TSkinScrollBarItem = (
    ssbiBackground,
    ssbiUpBtn,
    ssbiSlider,
    ssbiDownBtn,
    ssbiMark
    );

  TSkinCell = (scDefaultBackground, scSelectBackground, scHoverBackground, csFocusBackground, csComplexFocusBackground, scComplexSelBackground);


  TSkinNodeButton = (snbExpanded, snbHoverExpanded, snbCollapse, snbHoverCollapse);

  TSkinObjectElement = (
    soeHeaderButton,
    soeRowCheckImage
  );

  TSkinViewItem = (
    eviHeader
  );

  // 元素方向
  TElementDiection = (edLeft, edTop, edRight, edBottom);

  /// 页签
  TSkinTabState = (
      stsNormal,
      stsHot,
      stsPressed,
      stsActive
      );

  TSkinTabElement = record
    Display: TRect;
    State: TSkinTabState;
  end;

  TElementIcon = (
    eiRoundPoint     // 空心圆点
  );


  TTreeViewSkin = class
  private
    FBrush: TBrush;
    FDottedBrush: HBRUSH;
    FFont:TFont;
    FIsEmptyRes: Boolean;
    function BuildDotBrush: HBRUSH;
    procedure DrawImage(ACanvas:TCanvas; const ARect:TRect; const AOrgPlace, ASize:TPoint; XOffset, YOffset:integer);
    function GetResourceData(ID: TResourceAreas): TSkinElementData;
  public
    FData:TBitmap;
    constructor Create;
    destructor Destroy; override;

    class procedure DrawIcon(DC: HDC; R: TRect; ASrc: TBitmap; const Opacity: Byte = 255);

    procedure Draw(DC: HDC; const AData: TSkinElementData; const R: TRect);
    procedure DrawStretchWidth(DC: HDC; const AData: TSkinElementData; const R: TRect; s: TPoint);
    procedure DrawStretchHeight(DC: HDC; const AData: TSkinElementData; const R: TRect; s: TPoint);

    procedure DrawStretchElement(ACanvas: TCanvas; const ASrc, ADest, b: TRect);
    function  GetSize(Element:TSkinObjectElement):TPoint;

    procedure DrawFrame(dc:HDC; ARect: TRect; ASides: TRangeSides);
    procedure DrawElement(Element: TSkinNodeButton; ACanvas: TCanvas; ARect:TRect; Opacity: Byte = 255); overload;
    procedure DrawElement(Element: TSkinCell; ACanvas: TCanvas; ARect: TRect; ABKColor: TColor); overload;
    procedure DrawElement(Element: TSkinIndicator; ACanvas: TCanvas; ARect: TRect); overload;
    procedure DrawElement(Element: TSkinFrame; ACanvas: TCanvas; ARect: TRect); overload;
    procedure DrawElement(Element: TSkinHeader; ACanvas: TCanvas; ARect: TRect; AHasLine: Boolean = true); overload;
    procedure DrawElement(Element: TSkinButton; ACanvas: TCanvas; ARect: TRect); overload;
    procedure DrawElement(Element: TSkinTreeView; ACanvas: TCanvas; ARect: TRect); overload;
    procedure DrawElement(Element: TSkinDataView; ACanvas: TCanvas; ARect: TRect); overload;
    procedure DrawElement(Element: TSkinRangeFrame; ACanvas: TCanvas; ARect: TRect; ASides:TRangeSides; AHandlebars:TRangeHandle); overload;
    procedure DrawElement(Element: TSkinViewItem; DC:HDC; R:TRect; const AStr:string); overload;
    procedure DrawElement(E:TSkinScrollBarItem; AKind: TSkinScrollType; DC: HDC; const R: TRect; State: TSkinElemenState); overload;


    function  MeasureSize(const AText:string; const AFontname:TFontName; AFontSize:Integer):Integer; overload;
    function  MeasureSize(const AText:string; AFont: TFont):Integer; overload;

    function  PrepareDrawTab(ADC: HDC; ABk:TColor): TGPGraphics;
    procedure DrawTabBackground(AGraphics: TGPGraphics; const R: TRect; ADir: TElementDiection);
    procedure DrawTabItem(AGraphics: TGPGraphics; const ACaption: String; const R: TRect; AState: TSkinTabState; ADir: TElementDiection; Alignment: Boolean); overload;
    procedure DrawTabItem(AGraphics: TGPGraphics; AFont: TGPFont; const ACaption: String; const R: TRect; AState: TSkinTabState; ADir: TElementDiection; Alignment: Boolean); overload;
    procedure DrawWindowBackground(ADC: HDC; const R: TRect);
    procedure DrawAttachFloatBar(ADC: HDC; R: TRect);

    procedure DrawIcon9(Element: TElementIcon;  ADC: HDC; const R: TRect);
    procedure DrawHDotLine(ACanvas: TCanvas; Left, Right, Top: Integer; AColor: TColor);
    procedure DrawVDotLine(ACanvas: TCanvas; Top, Bottom, Left: Integer; AColor: TColor);

    class procedure DrawButtonBackground(DC: HDC; AState: TSkinIndicator; const R:TRect; const Opacity: Byte = 255); static;
    class procedure DrawButtonState(DC: HDC; AState: TSkinIndicator; const R:TRect; const AOpacity: Byte); static;
    procedure DrawFormButton(DC: HDC; AKind: TFormButtonKind; AState: TSkinIndicator; const R: TRect);
    procedure DrawElement(DC: HDC; AItem: TSkinToolbarElement; const R: TRect; const Opacity: Byte = 255); overload;

    property ResourceData[ID: TResourceAreas]: TSkinElementData read GetResourceData;


    property EmptyData:Boolean read FIsEmptyRes;
    function DataDC: HDC; inline;
  end;

  procedure DrawSplitLine(Handle:HDC; x1, y1, x2, y2:Integer);

  function TreeViewSkin:TTreeViewSkin;
  function mtUISkin: TTreeViewSkin;

procedure DrawGradient(DC:HDC; AStartColor, AEndColor: TColor; ARect: TRect; Vertical: Boolean); overload;

function Blend(Color1, Color2: TColor; A: Byte): TColor;
function FormatToARGB(v: TColor): Cardinal; inline;
function FormatToRGB(v: TColor): Cardinal; inline;

const
  SkinColor_Default_Font                    = $00444444;
  SkinColor_Default_background              = $00f0f0f0; //$00efedec;
  SkinColor_Form_background                 = SkinColor_Default_background;
  SkinColor_Toolbar_Background_Define       = SkinColor_Default_background;

  SkinColor_FormCaption_background          = $00bf7b18;

  SkinColor_Mainmenu_Active_Background      = $00bf7b18;
  SkinColor_MainMenu_Disable_Background     = $00e1c5b2;
  SkinColor_MainMenu_Font                   = $00ffffff;
  SkinColor_Frame_Line                      = $00d8bc94;
  SkinColor_Tree_Font                       = $00444444;  // TreView 默认字体颜色
  SkinColor_Tree_Line                       = $00e2e5e7;  // e1e1e1;
  SkinColor_Tree_StructureLine              = $00a0a0a0; //$00f4f6f8; //$00b5b7b9;  // 树形结构的样式线
                                              // $00a0a0a0
  SkinColor_Hint_Background                 = $00FFFFFF;
  SkinColor_Hint_Frame                      = SkinColor_Default_background;
  SkinColor_Hint_Text                       = SkinColor_Default_Font;

  SkinColor_Tab_Background                  = SkinColor_Default_background;
  SkinColor_Tab_BottomLine                  = $0094d8bc;//SkinColor_Frame_Line;// $00a6670c;
  SkinColor_TabItem_BG_Active               = $00a6670c;
  SkinColor_TabItem_Line_Active             = $00a6670c;
  SkinColor_TabItem_Font_Active             = $00ffffff;
  SkinColor_TabItem_BG_Hot                  = $00f2d5c2;
  SkinColor_TabItem_Line_Hot                = $00a6670c;
  SkinColor_TabItem_Font_Hot                = $00444444;
  SkinColor_TabItem_BG_Def                  = $00ffffff;
  SkinColor_TabItem_Line_Def                = $00f2d5c2;
  SkinColor_TabItem_Font_Def                = $00444444;


  SkinColor_TableHeader_Background_B        = $00fcfcfc;
  SkinColor_TableHeader_Background          = $00fcf7f3;
  SkinColor_TableHeader_BkgHot              = {$ifdef HeaderGradient} $00f5e6dc; {$else} $00f2d5c2; {$endif}
  SkinColor_TableHeader_BkgPressed          = {$ifdef HeaderGradient} $00f5e6dc; {$else} $00e3bda3; {$endif}
  SkinColor_TableHeader_BkgActiveLine       = $00f4933e;

  SkinColor_TableHeader_Line                = $00f2d5c2;
  SkinColor_TableHeader_Line_Active         = $00fcf7f3;
  SkinColor_TableHeader_Line_shadow         = $00fcfcfc;
  SkinColor_TableHeader_Line_Shadow_Active  = $00f6e2d4;

  SkinColor_Scrollbar_Background            = $00ffffff;



  SkinData_SIZE_ClickBox      = 13; ///

  /// 框选区域边线颜色
  SKINCOLOR_RANGEFRAME_SIDELINE = $00cc9900;//$00ca8b42;
  SKINCOLOR_RANGEFRAME_SIDELINE_INACTIVE = $00DdDdDd;

  TABCONTENT_SIDESPACE  = 6;              /// Tab 显示内容边距

  TABCAPTION_FONTSIZE   = 9;
  TAB_SPACE             = 0;              /// Tab 之间的间距
  TABSET_HEIGHT         = 26;             //  Tab set的默认高度
  TABSET_HIDESIZE       = 24;             //  隐藏视图页签尺寸，在fraSplitterView中有隐藏子项时会显示的页签

  TAB_DEFAULT_MARGINSTART = 3;
  TAB_DEFAULT_MARGINTOP = 2;
  // 绘制底线的实际高度
  TAB_MARGINBOTTOMLINEHEIGHT: array [Boolean] of integer = (0, 1);


  SPLITTER_SIZE         = 3;              //  分割条的宽度
  FRAMELINE_SIZE        = 1;              //  View 有线框是需要保留的尺寸


  SkinDlg_FontSize_Hint = 9;             // 对话框提示信息字体大小

  AllRangeSide = [rsdLeft, rsdTop, rsdRight, rsdBottom];

const
  SKINCOLOR_BTNHOT      = $00F2D5C2;  // Hot 激活状态
  SKINCOLOR_BTNPRESSED  = $00E3BDA3;  // 按下状态
  SIZE_FORMSYSBTN: TSize    = (cx: 29; cy: 18);
  SIZE_FORMFRAME: TRect     = (Left: 4; Top: 29; Right: 5; Bottom: 5); // 窗体边框的尺寸
  SPACE_FORMAREA            = 3;          // 功能区域之间间隔
  SIZE_FORMHEIGHTTOOLBAR    = 16;


const
  // 树标题工具条Action
  BASE_TREEHEADERACTION_ICONID = 203;
  ICONID_TREEHEADER_EXPAND1   = BASE_TREEHEADERACTION_ICONID + 0;
  ICONID_TREEHEADER_EXPAND2   = BASE_TREEHEADERACTION_ICONID + 1;
  ICONID_TREEHEADER_EXPAND3   = BASE_TREEHEADERACTION_ICONID + 2;
  ICONID_TREEHEADER_EXPAND4   = BASE_TREEHEADERACTION_ICONID + 3;
  ICONID_TREEHEADER_EXPANDFB  = BASE_TREEHEADERACTION_ICONID + 4;
  ICONID_TREEHEADER_EXPANDQD  = BASE_TREEHEADERACTION_ICONID + 5;
  ICONID_TREEHEADER_EXPANDDE  = BASE_TREEHEADERACTION_ICONID + 6;
  ICONID_TREEHEADER_SWITCH    = BASE_TREEHEADERACTION_ICONID + 7;



var
  SkinFontName_Default: string;
  SkinFontSize_Default:integer = 9;
  TABCAPTION_FONTNAME: string;

implementation

uses
  uUIRes;

const
  ResAreas: array [TResourceAreas] of TSkinElementData = (
    (X: 0;    Y: 133; W: 16; H: 16),   // checkbox 和 Radiobox 属于一组
    (X: 190;  Y: 182; W: 13; H: 11),   // 滚动条
    (X: 0;    Y: 69 ; W: 16; H: 16),   // 窗体系统按钮
    (X: 0;    Y: 85 ; W: 9 ; H: 16)    // 窗体Toolbar
  );

type
  TColorPair = record
    c1, c2: TColor;
  end;

  TabStyleData = record
    Frame:TGPPen;
    Background:TGPBrush;
    Font: TGPFont;
    Format: TGPStringFormat;
    Caption:TGPBrush;
    CanDrawFrame:Boolean;
    class procedure Clear(const AItem: TabStyleData); static;
    class procedure Build(AState: TSkinTabState; var AItem: TabStyleData); static;
  end;

  ElementStyles = class
    class var Tabs: array [TSkinTabState] of TabStyleData;
    class var TabBottomLine : TGPPen;

    class procedure Clear; static;
    class procedure Load; static;
  end;

var
  FTreeViewSkin:TTreeViewSkin;

function GetDefaultFont:string;
const
  cst_MicrElegantBlack = '微软雅黑';
  cst_Song = '宋体';
  cst_Song_En = 'SimSun';
  cst_Code = 'Consolas';
var
  sFontName: string;
begin
  sFontName := cst_Song;

//  if Screen.Fonts.IndexOf(cst_Code) <> -1 then
//    sFontName := cst_Code
//  else
  if Screen.Fonts.IndexOf(cst_MicrElegantBlack) <> -1 then
    sFontName := cst_MicrElegantBlack
  else if Screen.Fonts.IndexOf(cst_Song) <> -1 then
    sFontName := cst_Song
  else if Screen.Fonts.IndexOf(cst_Song_En) <> -1 then
    sFontName := cst_Song_En;

  Result := sFontName;
end;

procedure FillColor(ACanvas: TCanvas; ARect: TRect; AColor: TColor); inline;
begin
  ACanvas.Brush.Color := AColor;
  ACanvas.FillRect(ARect);
end;

procedure DrawHLine(ACanvas: TCanvas; AColor: TColor; x1, x2: integer; p: Integer); overload; inline;
begin
  with ACanvas do
  begin
    Pen.Color := AColor;
    MoveTo(x1, p);
    LineTo(x2, p);
  end;
end;

procedure DrawHLine(ACanvas: TCanvas; x1, x2: integer; p: Integer); overload; inline;
begin
  with ACanvas do
  begin
    MoveTo(x1, p);
    LineTo(x2, p);
  end;
end;

procedure DrawVLine(ACanvas: TCanvas; AColor: TColor; y1, y2: integer; p: Integer); overload; inline;
begin
  with ACanvas do
  begin
    Pen.Color := AColor;
    MoveTo(p, y1);
    LineTo(p, y2);
  end;
end;

procedure DrawVLine(ACanvas: TCanvas; y1, y2: integer; p: Integer); overload; inline;
begin
  with ACanvas do
  begin
    MoveTo(p, y1);
    LineTo(p, y2);
  end;
end;

function TreeViewSkin:TTreeViewSkin;
begin
  if FTreeViewSkin = nil then
    FTreeViewSkin := TTreeViewSkin.Create;
  Result := FTreeViewSkin;
end;

function mtUISkin: TTreeViewSkin;
begin
  if FTreeViewSkin = nil then
    FTreeViewSkin := TTreeViewSkin.Create;
  Result := FTreeViewSkin;
end;

type
  TRectSpace = (
    rsTopLeft, rsTopMid, rsTopRight,
    rsCenterLeft, rsCenterMid, rsCenterRight,
    rsBottomLeft, rsBottomMid, rsBottomRight);
//
function GetRectSpace(const R, B:TRect; ASpace:TRectSpace ):TRect; inline;
begin
  case ASpace of
    rsTopLeft:
    begin
      Result.Left   := R.Left;
      Result.Right  := R.Left + B.Left;
      Result.Top    := R.Top;
      Result.Bottom := R.Top + B.Top;
    end;
    rsTopMid:
    begin
      Result.Left   := R.Left + B.Left;
      Result.Right  := R.Right - B.Right;
      Result.Top    := R.Top;
      Result.Bottom := R.Top + B.Top;
    end;
    rsTopRight:
    begin
      Result.Left   := R.Right - B.Right;
      Result.Right  := R.Right;
      Result.Top    := R.Top;
      Result.Bottom := R.Top + B.Top;
    end;

    rsCenterLeft:
    begin
      Result.Left   := R.Left;
      Result.Right  := R.Left   + b.Left;
      Result.Top    := R.Top    + b.Top;
      Result.Bottom := R.Bottom - b.Bottom;
    end;
    rsCenterMid:
    begin
      Result.Left   := R.Left   + B.Left;
      Result.Right  := R.Right  - b.Right;
      Result.Top    := R.Top    + b.Top;
      Result.Bottom := R.Bottom - b.Bottom;
    end;
    rsCenterRight:
    begin
      Result.Left   := R.Right  - B.Right;
      Result.Right  := R.Right;
      Result.Top    := R.Top    + b.Top;
      Result.Bottom := R.Bottom - b.Bottom;
    end;

    rsBottomLeft:
    begin
      Result.Left   := R.Left;
      Result.Right  := R.Left + B.Left;
      Result.Top    := R.Bottom - b.Bottom;
      Result.Bottom := R.Bottom;
    end;
    rsBottomMid:
    begin
      Result.Left   := R.Left + B.Left;
      Result.Right  := R.Right - B.Right;
      Result.Top    := R.Bottom - b.Bottom;
      Result.Bottom := R.Bottom;
    end;
    rsBottomRight:
    begin
      Result.Left   := R.Right - B.Right;
      Result.Right  := R.Right;
      Result.Top    := R.Bottom - b.Bottom;
      Result.Bottom := R.Bottom;
    end;
    else
    begin
       Result.Left := 0;
       Result.Top := 0;
       Result.Right := 0;
       Result.Bottom := 0;// := Rect(0,0,0,0);
    end;
  end;
end;

procedure TTreeViewSkin.DrawStretchElement(ACanvas: TCanvas; const ASrc, ADest,
    b: TRect);
var
  cDest: HDC;
  cSrc: HDC;

  procedure BitDraw(ASpace:TRectSpace);
  var
    rDes: TRect;
    rSrc: TRect;
  begin
    rDes := GetRectSpace(ADest, b, ASpace);
    if rDes.Left < rDes.Right then
    begin
      rSrc := GetRectSpace(ASrc, b, ASpace);
      BitBlt(cDest, rDes.Left, rDes.Top, rDes.Width, rDes.Height,
           cSrc, rSrc.Left, rSrc.Top, SRCCOPY);
    end;
  end;

  procedure StretchDraw(ASpace:TRectSpace);
  var
    rDes: TRect;
    rSrc: TRect;
  begin
    rDes := GetRectSpace(ADest, b, ASpace);
    if rDes.Left < rDes.Right then
    begin
      rSrc := GetRectSpace(ASrc, b, ASpace);
      StretchBlt(cDest, rDes.Left, rDes.Top, rDes.Width, rDes.Height,
               cSrc, rSrc.Left, rSrc.Top, rSrc.Width, rSrc.Height, SRCCOPY);
    end;
  end;
begin
  cSrc := FData.Canvas.Handle;
  cDest := ACanvas.Handle;

  if b.Top > 0 then
  begin
    if b.Left > 0 then
      BitDraw(rsTopLeft);
    StretchDraw(rsTopMid);
    if b.Right > 0 then
      BitDraw(rsTopRight);
  end;

  // 中间位置
  if b.Left > 0 then
    StretchDraw(rsCenterLeft);
  StretchDraw(rsCenterMid);
  // 右中边
  if b.Right > 0 then
    StretchDraw(rsCenterRight);

  if b.Bottom > 0 then
  begin
    if b.Left > 0 then
      BitDraw(rsBottomLeft);
    StretchDraw(rsBottomMid);
    if b.Right > 0 then
      BitDraw(rsBottomRight);
  end;
end;

procedure TTreeViewSkin.Draw(DC: HDC; const AData: TSkinElementData; const R: TRect);
var
  P: TPoint;
begin
  P := R.TopLeft;
  p.X := P.X + (r.Width - AData.W) div 2;
  p.Y := p.Y + (r.Height - AData.H) div 2;
  BitBlt(DC, P.X, p.Y, AData.W, AData.H, DataDC, AData.X, AData.Y, SRCCOPY);
end;

procedure TTreeViewSkin.DrawStretchHeight(DC: HDC; const AData: TSkinElementData; const R: TRect; s: TPoint);
var
  x: Integer;
begin
  x := r.Left + (r.Width - AData.W) div 2;
  if s.X > 0 then
    BitBlt(DC, x, r.Top, AData.W, s.X, DataDC, AData.X, AData.Y, SRCCOPY);
  if s.Y > 0 then
    BitBlt(DC, x, r.Bottom - s.Y, AData.W, s.X, DataDC, AData.X, AData.Y + AData.H - s.Y, SRCCOPY);
  StretchBlt(DC, x, r.Top + s.X, AData.W, r.Height - s.X - s.Y, DataDC, AData.X, AData.Y + s.X, AData.W, AData.H - s.X - s.Y, SRCCOPY);
end;

procedure TTreeViewSkin.DrawStretchWidth(DC: HDC; const AData: TSkinElementData; const R: TRect; s: TPoint);
var
  y: integer;
begin
  y := r.Top + (r.Height - AData.H) div 2;
  if s.X > 0 then
    BitBlt(DC, r.Left, y, s.X, AData.H, DataDC, AData.X, AData.Y, SRCCOPY);
  if s.Y > 0 then
    BitBlt(DC, r.Right - s.Y, y, s.Y, AData.H, DataDC, AData.X + AData.W - s.Y, AData.Y, SRCCOPY);
  StretchBlt(DC, r.Left + s.X, y, r.Width - s.x - s.Y, AData.H, DataDC, AData.X + s.X, AData.Y, AData.W - s.X - s.Y, AData.H, SRCCOPY);
end;

function BuildTabFrameLine(const R:TGPRect; ADir: TElementDiection; var APoints: TPathPoints):integer;
begin
  Result := 4;
  with r do
    case ADir of
      edLeft:
      begin
        APoints[0] := MakePoint(X + Width - 1,  Y + Height - 1);
        APoints[1] := MakePoint(X,              Y + Height - 1);
        APoints[2] := MakePoint(X,              Y);
        APoints[3] := MakePoint(X + Width - 1,  Y);
      end;
      edTop:
      begin
        APoints[0] := MakePoint(X,              Y + Height - 1);
        APoints[1] := MakePoint(X,              Y);
        APoints[2] := MakePoint(X + Width - 1,  Y);
        APoints[3] := MakePoint(X + Width - 1,  Y + Height - 1);
      end;
      edRight:
      begin
        APoints[0] := MakePoint(X,              Y);
        APoints[1] := MakePoint(X + Width - 1,  Y);
        APoints[2] := MakePoint(X + Width - 1,  Y + Height - 1);
        APoints[3] := MakePoint(X,              Y + Height - 1);
      end;
      edBottom:
      begin
        APoints[0] := MakePoint(X + Width - 1,  Y);
        APoints[1] := MakePoint(X + Width - 1,  Y + Height - 1);
        APoints[2] := MakePoint(X,              Y + Height - 1);
        APoints[3] := MakePoint(X,              Y);
      end;
    end;
end;


procedure TTreeViewSkin.DrawTabBackground(AGraphics: TGPGraphics; const R: TRect; ADir: TElementDiection);
var
  cPen: TGPPen;
begin
  cPen := ElementStyles.TabBottomLine;// Tabs[stsNormal].Frame;//
  case ADir of
    edTop     : AGraphics.DrawLine(cPen, r.Left,       r.Bottom -1,  r.Right,       r.Bottom - 1) ;
    edBottom  : AGraphics.DrawLine(cPen, r.Left,       r.Top,        r.Right,       r.Top) ;
    edLeft    : AGraphics.DrawLine(cPen, r.Right - 1,  r.Top,        r.Right - 1,   r.Bottom) ;
    edRight   : AGraphics.DrawLine(cPen, r.Left,       r.Top,        r.Left,        r.Bottom) ;
  end;
end;

procedure TTreeViewSkin.DrawTabItem(AGraphics: TGPGraphics; AFont: TGPFont;
  const ACaption: String; const R: TRect; AState: TSkinTabState;
  ADir: TElementDiection; Alignment: Boolean);
var
  rArea: TGPRect;
  iSpace:integer;
  iSaveFlag:Cardinal;
  rFrame:TPathPoints;
  iFrameCnt:integer;
  rContent: TGPRectF;
begin
  iSpace := 0;
  if not Alignment and (stsActive <> AState) then
    inc(iSpace, 2);

  with r do
    case ADir of
      edLeft    : rArea := MakeRect(Left + iSpace, Top, Width - iSpace, Height);
      edTop     : rArea := MakeRect(left, Top + iSpace, Width, Height - iSpace);
      edRight   : rArea := MakeRect(Left, Top, Width - iSpace, Height);
      edBottom  : rArea := MakeRect(Left, Top, Width, Height - iSpace);
      else rArea := MakeRect(left, Top + iSpace, Width, Height - iSpace);
    end;

  with ElementStyles.Tabs[AState] do
  begin
    AGraphics.FillRectangle(Background, rArea);
    if CanDrawFrame then
    begin
      iFrameCnt := BuildTabFrameLine(rArea, ADir, rFrame);
      AGraphics.DrawLines(Frame, PGPPoint(@rFrame), iFrameCnt);
    end;

    if ACaption <> '' then
    begin
      iSaveFlag := 0;
      if ADir in [edLeft, edRight] then
      begin
        iSaveFlag := Format.GetFormatFlags;
        Format.SetFormatFlags(iSaveFlag or StringFormatFlagsDirectionVertical);
      end;

      rContent.X := rArea.X;
      rContent.Y := rArea.Y;
      rContent.Width := rArea.Width;
      rContent.Height := rArea.Height;
      /// 微调文字显示, 靠近线
      case ADir of
        edLeft:
        begin
          rContent.X := rContent.X + 2;
          rContent.Width := rContent.Width - 2;
        end;
        edTop :
        begin
          rContent.y := rContent.Y + 4;
          rContent.Height := rContent.Height - 4;
        end;
        edRight:
        begin
          rContent.X := rContent.X - 2;
          rContent.Width := rContent.Width - 2;
        end;
      end;

      AGraphics.DrawString(ACaption, Length(ACaption), AFont, rContent, Format, Caption);
      if ADir in [edLeft, edRight] then
        Format.SetFormatFlags(iSaveFlag);
    end;
  end;
end;

procedure TTreeViewSkin.DrawTabItem(AGraphics: TGPGraphics;
    const ACaption: String; const R: TRect; AState: TSkinTabState;
    ADir: TElementDiection; Alignment: Boolean);
begin
  DrawTabItem(AGraphics, ElementStyles.Tabs[AState].Font, ACaption, r, AState, ADir, Alignment);
end;

procedure TTreeViewSkin.DrawWindowBackground(ADC: HDC; const R: TRect);
var
  hb: HBRUSH;
begin
  //ColorToRGB()
  hb := CreateSolidBrush(FormatToRGB(SkinColor_Form_background));
  FillRect(ADC, r, hb);
  DeleteObject(hb);
end;

procedure DrawSplitLine(Handle:HDC; x1, y1, x2, y2:integer);
var
  g:TGPGraphics;
  p:TGPPen;
begin
  g := TGPGraphics.Create(Handle);
  p := TGPPen.Create(MakeColor(200, 152, 152, 152), 2);
  p.SetDashStyle(DashStyleDot);
  g.DrawLine(p, x1, y1, x2, y2);
  p.Free;
  g.Free;
end;

procedure DrawTransparentBitmap(Source: TBitmap; sx, sy: Integer; Destination: HDC;
  const dX, dY: Integer;  w, h: Integer; const Opacity: Byte); overload;
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

procedure DrawTransparentBitmap(Source: TBitmap; Destination: TCanvas;
  const X, Y: Integer; const Opacity: Byte); overload;
begin
  DrawTransparentBitmap(Source, 0, 0, Destination.Handle, x, y, Source.Width, source.Height, Opacity);
end;

procedure DrawGradient(DC:HDC; AStartColor, AEndColor: TColor; ARect: TRect; Vertical: Boolean); overload;
var
  StartColor, EndColor: Cardinal;
  GradientRect: TGradientRect;
  Vertexes: array [0 .. 1] of TTriVertex;
begin
  StartColor := ColorToRGB(AStartColor);
  EndColor := ColorToRGB(AEndColor);

  Vertexes[0].X := ARect.Left;
  Vertexes[0].Y := ARect.Top;
  Vertexes[0].Red := GetRValue(StartColor) shl 8;
  Vertexes[0].Blue := GetBValue(StartColor) shl 8;
  Vertexes[0].Green := GetGValue(StartColor) shl 8;
  Vertexes[0].Alpha := 0;

  Vertexes[1].X := ARect.Right;
  Vertexes[1].Y := ARect.Bottom;
  Vertexes[1].Red := GetRValue(EndColor) shl 8;
  Vertexes[1].Blue := GetBValue(EndColor) shl 8;
  Vertexes[1].Green := GetGValue(EndColor) shl 8;
  Vertexes[1].Alpha := 0;

  GradientRect.UpperLeft := 0;
  GradientRect.LowerRight := 1;
  GradientFill(DC, @Vertexes[0], 2, @GradientRect, 1, Integer(Vertical));
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Blend(Color1, Color2: TColor; A: Byte): TColor;
var
  c1, c2: LongInt;
  r, g, b, v1, v2: Byte;
begin
  A := Round(2.55 * A);
  c1 := ColorToRGB(Color1);
  c2 := ColorToRGB(Color2);
  v1 := Byte(c1);
  v2 := Byte(c2);
  r := Byte(A * (v1 - v2) shr 8 + v2);
  v1 := Byte(c1 shr 8);
  v2 := Byte(c2 shr 8);
  g := Byte(A * (v1 - v2) shr 8 + v2);
  v1 := Byte(c1 shr 16);
  v2 := Byte(c2 shr 16);
  b := Byte(A * (v1 - v2) shr 8 + v2);
  Result := (b shl 16) + (g shl 8) + r;
end;

{$WARN COMBINING_SIGNED_UNSIGNED OFF}
function FormatToARGB(v: TColor): Cardinal; inline;
begin
  Result := $ff000000 or
            (v and $ff0000 shr 16) or
            (v and $ff00) or
            (v and $FF shl 16);
end;

function FormatToRGB(v: TColor): Cardinal;
begin
  Result := (v and $ff0000 shr 16) or
            (v and $ff00) or
            (v and $FF shl 16);
end;
{$WARN COMBINING_SIGNED_UNSIGNED ON}


{ TTreeViewSkin }

constructor TTreeViewSkin.Create;
begin
  inherited;
  FData := TBitmap.Create;
  //FData.AlphaFormat := afDefined;
  //LoadSkinRes('Tree', FData);
  UIRes.LoadBitmap('TreeStyleGraph', FData);
  FIsEmptyRes := FData.Empty;
  FBrush := TBrush.Create;
  FFont := TFont.Create;
  FDottedBrush := BuildDotBrush;
end;


procedure TTreeViewSkin.DrawElement(Element: TSkinTreeView; ACanvas: TCanvas;
    ARect: TRect);
begin

end;

function TTreeViewSkin.DataDC: HDC;
begin
  Result := FData.Canvas.Handle;
end;

destructor TTreeViewSkin.Destroy;
begin
  FData.Free;
  FBrush.Free;
  FFont.Free;
  DeleteObject(FDottedBrush);
  inherited;
end;


procedure TTreeViewSkin.DrawAttachFloatBar(ADC: HDC; R: TRect);
var
  hb: HBRUSH;
begin
  hb := CreateSolidBrush(FormatToRGB(SkinColor_TableHeader_Background));
  FillRect(adc, r, hb);
  DeleteObject(hb);
end;

procedure TTreeViewSkin.DrawElement(Element: TSkinDataView; ACanvas: TCanvas;
    ARect: TRect);
begin
end;

function TTreeViewSkin.GetResourceData(ID: TResourceAreas): TSkinElementData;
begin
  Result := ResAreas[ID];
end;

function TTreeViewSkin.GetSize(Element: TSkinObjectElement): TPoint;
const
  ELEMENTSIZES:array [TSkinObjectElement] of TPoint = (
    (X:16; Y:16),
    (X:16; Y:16)
  );
begin
  Result := ELEMENTSIZES[Element];
end;

function TTreeViewSkin.MeasureSize(const AText: string;
  AFont: TFont): Integer;
var
  DC: HDC;
  rSize: TSize;
  SaveFont: HFont;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, AFont.Handle);
  Winapi.Windows.GetTextExtentPoint32(DC, AText, Length(AText), rSize);
  SelectObject(DC, SaveFont);
  Result := rSize.cx;
end;

function TTreeViewSkin.MeasureSize(const AText: string;
  const AFontname: TFontName; AFontSize: Integer): Integer;
var
  cFont: TFont;
begin
  cFont := TFont.Create;
  cFont.Name := AFontname;
  cFont.Size := AFontSize;

  Result := MeasureSize(AText, cFont);
  cFont.Free;
end;

function TTreeViewSkin.PrepareDrawTab(ADC: HDC; ABk:TColor): TGPGraphics;
begin
  Result := TGPGraphics.Create(ADC);
  Result.Clear(ColorRefToARGB(ABk));
//  Result.Clear(aclWhite);
  Result.SetSmoothingMode(SmoothingModeInvalid);
end;

procedure TTreeViewSkin.DrawElement(Element: TSkinNodeButton; ACanvas: TCanvas;
    ARect:TRect; Opacity: Byte = 255);
var
  rSrc:TRect;
  rDes:TRect;
begin
  case Element of
    snbExpanded       :rSrc := Rect(240, 23, 240 + 11, 23 + 11);
    snbCollapse       :rSrc := Rect(251, 23, 251 + 11, 23 + 11);
    snbHoverExpanded  :rSrc := Rect(273, 23, 273 + 11, 23 + 11);
    snbHoverCollapse  :rSrc := Rect(262, 23, 262 + 11, 23 + 11);
  else
    rSrc := Rect(0,0,0,0);
  end;

  rDes := ARect;
  rDes.Right := rDes.Left + 11;
  rDes.Bottom := rDes.Top + 11;
  DrawTransparentBitmap(FData, rSrc, ACanvas, rDes, Opacity);
end;

procedure TTreeViewSkin.DrawElement(Element: TSkinCell; ACanvas: TCanvas;
    ARect: TRect; ABKColor: TColor);
const
  COLOR_SELECTCELL:array [TSkinCell] OF TColor =
    (clNone,      // scDefaultBackground,
     $00fcf7f3,   // scSelectBackground
     $00FFF7EE,   // scHoverBackground
     $00FFFFFF,   // csFocusBackground,
     $00FFFFFF,   // csComplexFocusBackground
     $00c9c9c9    // scComplexSelBackground
     );

  function BleanSelectColor(c1, c2: TColor):TColor;
  var
    iClr: TColor;
  begin
    iClr := c1;
    if (c1 = clNone) or (c1 = clDefault) then
      iClr := c2
    else if (c2 <> clNone) and (c2 <> clDefault) then
      iClr := Blend(c1, c2, 65);
    Result := iClr;
  end;
begin
  ACanvas.Brush.Color :=  BleanSelectColor(ABKColor, COLOR_SELECTCELL[Element]); //);// $00FFFFFF; // EFFEFE;
  Acanvas.FillRect(ARect);

  case Element of
    csComplexFocusBackground:
    begin
      ACanvas.Pen.Color := $00909090;
      ACanvas.Pen.Width := 1;
      ACanvas.Rectangle(ARect.Left + 1, ARect.Top + 1, ARect.Right - 1, ARect.Bottom - 1);
    end;
   end;
end;

procedure TTreeViewSkin.DrawElement(Element: TSkinIndicator; ACanvas: TCanvas; ARect: TRect);
const
  INDCOLORS:array [TSkinIndicator] of TColorPair = (
    //siInactive, siHover, siPressed, siSelected, siHoverSelected
    (c1:SkinColor_TableHeader_Background; c2:SkinColor_TableHeader_Line),
    (c1:SkinColor_TableHeader_BkgHot;     c2:SkinColor_TableHeader_Line_Active),
    (c1:SkinColor_TableHeader_BkgPressed; c2:SkinColor_TableHeader_Line_Active),
    (c1:SkinColor_TableHeader_BkgHot;     c2:SkinColor_TableHeader_Line_Active),
    (c1:SkinColor_TableHeader_BkgHot;     c2:SkinColor_TableHeader_Line_Active)
  );
begin
  ACanvas.Brush.Color := INDCOLORS[Element].c1;
  ACanvas.Pen.Color := INDCOLORS[Element].c2;
  ACanvas.FillRect(ARect);
  ACanvas.MoveTo(ARect.left, ARect.Bottom - 1);
  ACanvas.LineTo(ARect.Right - 1, ARect.Bottom - 1);
  ACanvas.Pen.Color := SkinColor_TableHeader_Line;
  ACanvas.LineTo(ARect.Right - 1, ARect.Top - 1);
end;

procedure TTreeViewSkin.DrawElement(Element: TSkinFrame; ACanvas: TCanvas;
  ARect: TRect);
begin
  case Element of
    sfFlat:
    begin
      ACanvas.Pen.Color := $00F5F5F5;
      ACanvas.Pen.Style := psSolid;
      ACanvas.Brush.Color := $00F5F5F5;
      ACanvas.Rectangle(ARect);
    end ;
  end;
end;

procedure TTreeViewSkin.DrawElement(Element: TSkinHeader; ACanvas: TCanvas; ARect: TRect; AHasLine: Boolean = true);

  procedure DrawTopLine(c1, AShadow: TColor);
  begin
    ACanvas.Pen.Color := c1;
    ACanvas.MoveTo(ARect.Left, ARect.Top);
    ACanvas.LineTo(ARect.Right, ARect.Top);
    ACanvas.Pen.Color := AShadow;
    ACanvas.MoveTo(ARect.Left, ARect.Top+1);
    ACanvas.LineTo(ARect.Right, ARect.Top+1);
  end;

begin
  case Element of
    shBackground:
    begin
      ACanvas.Brush.Color := SkinColor_TableHeader_Background;
      ACanvas.FillRect(ARect);

      if AHasLine then
        DrawTopLine(SkinColor_TableHeader_Line, SkinColor_TableHeader_Line_shadow);
    end;

    shStartInactive,
    shMiddleInactive,
    shEndInactive:
    begin
      FillColor(ACanvas, ARect, SkinColor_TableHeader_Background);
      ACanvas.Pen.Color := SkinColor_TableHeader_Line;
      ACanvas.MoveTo(ARect.Right - 1, ARect.Top);
      ACanvas.LineTo(ARect.Right - 1, ARect.Bottom - 1);
      ACanvas.LineTo(ARect.Left - 1, ARect.Bottom - 1);

      if AHasLine then
        DrawTopLine(SkinColor_TableHeader_Line, SkinColor_TableHeader_Line_shadow);
    end;

    shStartPressed,
    shMiddlePressed,
    shEndPressed:
    begin
      FillColor(ACanvas, ARect, SkinColor_TableHeader_BkgPressed);
      ACanvas.Pen.Color := SkinColor_TableHeader_Line_Active;
      ACanvas.MoveTo(ARect.Right - 1, ARect.Top + 1 );
      ACanvas.LineTo(ARect.Right - 1, ARect.Bottom - 1);

      if AHasLine then
      begin
        ACanvas.Pen.Color := SkinColor_TableHeader_Line_Shadow_Active;
        ACanvas.MoveTo(ARect.Left, ARect.Top+1);
        ACanvas.LineTo(ARect.Right - 1, ARect.Top+1);
      end;
    end;

    shStartSelected,
    shMiddleSelected,
    shEndSelected:
    begin
      FillColor(ACanvas, ARect, SkinColor_TableHeader_BkgHot);
      ACanvas.Pen.Color := SkinColor_TableHeader_Line_Active;
      ACanvas.MoveTo(ARect.Right - 1, ARect.Top + 1);
      ACanvas.LineTo(ARect.Right - 1, ARect.Bottom - 1);

      if AHasLine then
      begin
        ACanvas.Pen.Color := SkinColor_TableHeader_Line_Shadow_Active;
        ACanvas.MoveTo(ARect.Left, ARect.Top+1);
        ACanvas.LineTo(ARect.Right - 1, ARect.Top+1);
      end;
    end;

    shStartHover,
    shMiddleHover,
    shEndHover:
    begin
      FillColor(ACanvas, ARect, SkinColor_TableHeader_BkgHot);
        ACanvas.Pen.Color := SkinColor_TableHeader_Line_Active;
        ACanvas.MoveTo(ARect.Right - 1, ARect.Top + 1);
        ACanvas.LineTo(ARect.Right - 1, ARect.Bottom - 1);

      if AHasLine then
      begin
        ACanvas.Pen.Color := SkinColor_TableHeader_Line_Shadow_Active;
        ACanvas.MoveTo(ARect.Left, ARect.Top+1);
        ACanvas.LineTo(ARect.Right - 1, ARect.Top+1);
      end;
    end;

    shStartHoverSelected,
    shMiddleHoverSelected,
    shEndHoverSelected:
    begin
      FillColor(ACanvas, ARect, SkinColor_TableHeader_Line_Active);
      ACanvas.Pen.Color := SkinColor_TableHeader_Line;
      ACanvas.MoveTo(ARect.Right - 1, ARect.Top + 1);
      ACanvas.LineTo(ARect.Right - 1, ARect.Bottom - 1);

      if AHasLine then
      begin
        ACanvas.Pen.Color := SkinColor_TableHeader_Line_Shadow_Active;
        ACanvas.MoveTo(ARect.Left, ARect.Top+1);
        ACanvas.LineTo(ARect.Right - 1, ARect.Top+1);
      end;
    end;

    shBtnCloseInactive, shBtnCloseHover, shBtnClosePressed:
      DrawImage(ACanvas, ARect, Point(300, 32), GetSize(soeHeaderButton), ord(Element) - ord(shBtnCloseInactive), 0);
    shBtnFilterNormal, shBtnFilterHot, shBtnFilterPressed:
      DrawImage(ACanvas, ARect, Point(300, 48), GetSize(soeHeaderButton), ord(Element) - ord(shBtnFilterNormal), 0);
    shBtnActiveFilterNormal, shBtnActiveFilterHot, shBtnActiveFilterPressed:
      DrawImage(ACanvas, ARect, Point(300, 64), GetSize(soeHeaderButton), ord(Element) - ord(shBtnActiveFilterNormal), 0);

    shHideColumnPrev:
    begin
      ACanvas.Brush.Color := $00cccccc;
      ACanvas.FillRect(Rect(ARect.Left, ARect.Top, ARect.Left+2, ARect.Bottom));
    end;
  end;
end;

procedure TTreeViewSkin.DrawElement(Element: TSkinButton; ACanvas: TCanvas;
    ARect: TRect);
const
  LEFT_CBX = 240;
  TOP_CBX  = 37;
  SIZE_CBX = 13;
//  CheckboxOff: TPoint = (X: 0; Y: 100);
//  RadioboxOff: TPoint = (X: 16; Y: 100);

  function CheckboxOff: TPoint;
  begin
    Result := ResourceData[raCheckbox].Offset;
  end;

  function RadioboxOff: TPoint;
  var
    d: TSkinElementData;
  begin
    d := ResourceData[raCheckbox];
    Result := d.Offset;
    inc(Result.X, d.W);
  end;


var
  rDes: TRect;
  rSrc:TRect;
  iSize:Integer;
  //rOff: TPoint;

  procedure SetParams(s, x, y, n: integer);
  begin
    iSize := s;
    rSrc.Left := x;
    rSrc.Top := y + ((n-1) * s);
    rSrc.Right := rSrc.Left + iSize;
    rSrc.Bottom := rSrc.Top + iSize;
  end;


begin
  iSize := 13;
  rSrc.Left := -1;

  case Element of
    sbEllipsisNormal,
    sbEllipsisHot,
    sbEllipsisPressed:
    begin
      iSize :=  16;
      rSrc.Left := 300 + iSize * (ord(Element) - ord(sbEllipsisNormal));
      rSrc.Top := 16;
    end;

    sbDropButtonNormal,
    sbDropButtonHot,
    sbDropButtonPressed,
    sbDropButtonDisabled:
    begin
      iSize :=  16;
      rSrc.Left := 300 + iSize * (ord(Element) - ord(sbDropButtonNormal));
      rSrc.Top := 16 * 5;
    end;

    sbCheckUncheckedNormal    : SetParams(16, CheckboxOff.X, CheckboxOff.Y, 1);
    sbCheckUncheckedHot,
    sbCheckUncheckedPressed   : SetParams(16, CheckboxOff.X, CheckboxOff.Y, 2);
    sbCheckUncheckedDisabled  : SetParams(16, CheckboxOff.X, CheckboxOff.Y, 5);

    sbCheckCheckedNormal,
    sbCheckCheckedHot,
    sbCheckCheckedPressed     : SetParams(16, CheckboxOff.X, CheckboxOff.Y, 3);
    sbCheckCheckedDisabled    : SetParams(16, CheckboxOff.X, CheckboxOff.Y, 7);

    sbCheckMixedNormal,
    sbCheckMixedHot,
    sbCheckMixedPressed       : SetParams(16, CheckboxOff.X, CheckboxOff.Y, 4);
    sbCheckMixedDisabled      : SetParams(16, CheckboxOff.X, CheckboxOff.Y, 8);

    sbRadioUnRadioedNormal    : SetParams(16, RadioboxOff.X, RadioboxOff.Y, 1);
    sbRadioUnRadioedHot,
    sbRadioUnRadioedPressed   : SetParams(16, RadioboxOff.X, RadioboxOff.Y, 2);
    sbRadioUnRadioedDisabled  : SetParams(16, RadioboxOff.X, RadioboxOff.Y, 5);

    sbRadioRadioedNormal,
    sbRadioRadioedHot,
    sbRadioRadioedPressed     : SetParams(16, RadioboxOff.X, RadioboxOff.Y, 3);
    sbRadioRadioedDisabled    : SetParams(16, RadioboxOff.X, RadioboxOff.Y, 8);
  end;

  if (rSrc.Left >= 0) and ((ARect.Right - ARect.Left) >= iSize) and ((ARect.Bottom - ARect.Top) >= iSize) then
  begin
    rSrc.Right := rSrc.Left + iSize;
    rSrc.Bottom := rSrc.Top + iSize;
    rDes.Left := ARect.Left + (ARect.Width - iSize) div 2;
    rDes.Top := ARect.Top + (ARect.Height - iSize) div 2;
    rDes.Right := rDes.Left + iSize;
    rDes.Bottom := rDes.Top + iSize;

    DrawTransparentBitmap(FData, rSrc, ACanvas, rDes, 255);
  end;

end;

procedure TTreeViewSkin.DrawIcon9(Element: TElementIcon; ADC: HDC; const R: TRect);
const
  OFFSET_ELEICON: TPoint = (X: 0; Y: 60);
var
  sx: Integer;
  sy: Integer;
  x: Integer;
  y: Integer;
begin
  x := r.Left;
  y := r.Top;
  if R.Width > 9 then
    x := x + (r.Width - 9) div 2;
  if r.Height > 9 then
    y := y + (r.Height - 9) div 2;

  sx := OFFSET_ELEICON.X + 9 * (ord(Element) - ord(Low(TElementIcon)));
  sy := OFFSET_ELEICON.Y;

  DrawTransparentBitmap(FData, sx, sy, ADC, x, y, 9, 9, 255);
end;

procedure TTreeViewSkin.DrawHDotLine(ACanvas: TCanvas; Left, Right, Top: Integer; AColor: TColor);
var
  R: TRect;
begin
  with ACanvas do
  begin
    Brush.Color := AColor;
    R := Rect(Min(Left, Right), Top, Max(Left, Right) + 1, Top + 1);
    Winapi.Windows.FillRect(Handle, R, FDottedBrush);
  end;
end;

procedure TTreeViewSkin.DrawVDotLine(ACanvas: TCanvas; Top, Bottom, Left: Integer; AColor: TColor);
var
  R: TRect;
begin
  with ACanvas do
  begin
    Brush.Color := AColor;
    R := Rect(Left, Min(Top, Bottom), Left + 1, Max(Top, Bottom) + 1);
    Winapi.Windows.FillRect(Handle, R, FDottedBrush);
  end;
end;

procedure TTreeViewSkin.DrawImage(ACanvas:TCanvas; const ARect:TRect; const
    AOrgPlace, ASize:TPoint; XOffset, YOffset:integer);
var
  rSrc: TRect;
begin
  // 绘制图标
  ///
  ///  参数
  ///    ASourcePlace   --- 贴图的所在位置
  ///    ASize          --- 元素的尺寸
  ///    XOffset        --- X方向偏移几个相同元素位置
  ///    YOffset        --- Y方向偏移几个相同元素位置
  ///
  rSrc.Left := AOrgPlace.X + XOffset * ASize.X;
  rSrc.Right := rSrc.Left + ASize.X;
  rSrc.Top := AOrgPlace.Y + YOffset * ASize.Y;
  rSrc.Bottom := rSrc.Top + ASize.Y;

  DrawTransparentBitmap(FData, rSrc, ACanvas, ARect, 255);
end;



procedure TTreeViewSkin.DrawElement(Element: TSkinRangeFrame; ACanvas: TCanvas;
    ARect: TRect; ASides:TRangeSides; AHandlebars:TRangeHandle);
const
  HandleSize = 5;
var
  rColor: TColor;

  procedure DrawDragHandle(h:HDC; x, y, dl,dt,dr,db:Integer);
  begin
    ACanvas.Brush.Color := clWhite;
    FillRect(h, Rect(x, y, x+HandleSize, y+HandleSize), ACanvas.Brush.Handle);
    ACanvas.Brush.Color := rColor;
    FillRect(h, Rect(x+dl, y+dt, x+dr+HandleSize, y+db+HandleSize), ACanvas.Brush.Handle);
  end;

begin
  rColor := SKINCOLOR_RANGEFRAME_SIDELINE;
  case Element of
    srfSelectInactiveFocused:
        rColor := SKINCOLOR_RANGEFRAME_SIDELINE_INACTIVE;
  end;
  ///
  ///  绘制选择框
  ///    由于绘制的框为单元格有效区域，未包含线横竖线的位置，
  ///    需要修正右侧和底部
  ///
  with ACanvas do
  begin
    Brush.Color := rColor;
    Brush.Style := bsSolid;

    // 修正边框
    if not (rsdRight in ASides) then
      inc(ARect.Right);
    if not (rsdBottom in ASides) then
      Inc(ARect.Bottom);

    if rsdLeft in ASides then
      Winapi.Windows.FillRect(Handle, Rect(ARect.Left, ARect.Top, ARect.Left+2, ARect.Bottom), Brush.Handle);
    if rsdTop in ASides then
      Winapi.Windows.FillRect(Handle, Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Top + 2), Brush.Handle);
    if rsdRight in ASides then
      Winapi.Windows.FillRect(Handle, Rect(ARect.Right - 2, ARect.Top, ARect.Right, ARect.Bottom), Brush.Handle);
    if rsdBottom in ASides then
      Winapi.Windows.FillRect(Handle, Rect(ARect.Left, ARect.Bottom - 2, ARect.Right, ARect.Bottom), Brush.Handle);

    if rhLeftTop in AHandlebars then
      DrawDragHandle(Handle, ARect.Left, ARect.Top, 0, 0, -1, -1);
    if rhLeftBottom in AHandlebars then
      DrawDragHandle(Handle, ARect.Left, ARect.Bottom - HandleSize, 0, 1, -1, 0);
    if rhRightTop in AHandlebars then
      DrawDragHandle(Handle, ARect.Right - HandleSize, ARect.Top, 1, 0, 0, -1);
    if rhRightBottom in AHandlebars then
      DrawDragHandle(Handle, ARect.Right - HandleSize, ARect.Bottom - HandleSize, 1, 1, 0, 0);
  end;
end;

procedure TTreeViewSkin.DrawElement(Element: TSkinViewItem; DC: HDC; R: TRect;
  const AStr: string);
var
  Flag: Cardinal;
  rText: TRect;
  sTitle: string;
  SaveFont: LongInt;
begin
  sTitle := AStr;
  FBrush.Color := $00ebebeb;
  FillRect(dc, r, FBrush.Handle);

  Flag := DT_NOPREFIX or DT_END_ELLIPSIS or DT_SINGLELINE;

  rText := r;
  rText.Left := 6;
  DrawText(DC, PChar(sTitle), length(sTitle), rText, Flag or DT_CALCRECT or DT_LEFT);
  rText.Right := Min(rText.Right + 12, r.Right);
  rText.Top := 0;
  rText.Bottom := r.Bottom;

  DrawGradient(Dc, $00ebebeb, $00ffffff, rText, True);

  FFont.Name := SkinFontName_Default; // '宋体';
  FFont.Style := [];
  FFont.Size := SkinFontSize_Default;// 9;

  SaveFont := SelectObject(DC, FFont.Handle);
  SetBkMode(DC, TRANSPARENT);
  inc(rText.Top, 2);
  DrawText(DC, PChar(sTitle), length(sTitle), rText, Flag or DT_CENTER or DT_VCENTER);
  SelectObject(DC, SaveFont);
end;

procedure TTreeViewSkin.DrawFrame(dc: HDC; ARect: TRect; ASides: TRangeSides);
var
  iPen: HPEN;
  iSaveObj: HGDIOBJ;
  pts : array [1..5] of TPoint;

  procedure DrawLine(x1, y1, x2, y2: Integer);
  begin
    pts[1].X := x1;  pts[1].Y := y1;
    pts[2].X := x2;  pts[2].Y := y2;
    Polyline(dc, pts, 2);
  end;
begin
  iPen := CreatePen(PS_SOLID, 1, ColorToRGB(SkinColor_Frame_Line));
  iSaveObj := SelectObject(DC, iPen);

  if ASides = [rsdLeft, rsdTop, rsdRight, rsdBottom] then
  begin
    with ARect do
    begin
      pts[1].X := Left;     pts[1].Y := Top;
      pts[2].X := Right-1;  pts[2].Y := Top;
      pts[3].X := Right-1;  pts[3].Y := Bottom -1;
      pts[4].X := Left;     pts[4].Y := Bottom -1;
      pts[5].X := Left;     pts[5].Y := Top;
    end;
    Polyline(dc, pts, 5);
  end
  else
  begin
    with ARect do
    begin
      if rsdLeft in ASides then
        DrawLine(left, top, left, Bottom);
      if rsdTop in ASides then
        DrawLine(Left, Top, Right, Top);
      if rsdRight in ASides then
        DrawLine(Right - 1, Top, Right - 1, Bottom);
      if rsdBottom in ASides then
        DrawLine(Left, Bottom - 1, Right, Bottom - 1);
    end;
  end;

  SelectObject(DC, iSaveObj);
  DeleteObject(iPen);
end;

class procedure ElementStyles.Clear;
var
  iTabIdx: TSkinTabState;
begin
  for iTabIdx := Low(TSkinTabState) to High(TSkinTabState) do
    TabStyleData.Clear(Tabs[iTabIdx]);
  TabBottomLine.Free;
end;


class procedure ElementStyles.Load;
var
  iTabIdx: TSkinTabState;
begin
  SkinFontName_Default := GetDefaultFont;
  //SkinFontSize_Default := 10;
  TABCAPTION_FONTNAME := SkinFontName_Default;


  for iTabIdx := Low(TSkinTabState) to High(TSkinTabState) do
    TabStyleData.Build(iTabIdx, Tabs[iTabIdx]);
  TabBottomLine := TGPPen.Create(FormatToARGB(SkinColor_Frame_Line));
end;

class procedure TabStyleData.Build(AState: TSkinTabState; var AItem: TabStyleData);
const
  COLORS:array [TSkinTabState] of array [1..3] of TColor = (
      (SkinColor_TabItem_Line_Def, SkinColor_TabItem_BG_Def, SkinColor_TabItem_Font_Def),
      (SkinColor_TabItem_Line_Hot, SkinColor_TabItem_BG_Hot, SkinColor_TabItem_Font_Hot),
      (SkinColor_TabItem_Line_Active, SkinColor_TabItem_BG_Active, SkinColor_TabItem_Font_Active),
      (SkinColor_TabItem_Line_Active, SkinColor_TabItem_BG_Active, SkinColor_TabItem_Font_Active)
    );
var
  flag: cardinal;
begin
  AItem.Frame       := TGPPen.Create(FormatToARGB(COLORS[AState][1]));
  AItem.Background  := TGPSolidBrush.Create(FormatToARGB(COLORS[AState][2]));
  AItem.Caption     := TGPSolidBrush.Create(FormatToARGB(COLORS[AState][3]));
  AItem.Font        := TGPFont.Create(TABCAPTION_FONTNAME, TABCAPTION_FONTSIZE);
  AItem.Format      := TGPStringFormat.Create;
  AItem.CanDrawFrame:= AState in [stsNormal, stsHot];

  with AItem.Format do
  begin
    SetAlignment(StringAlignmentCenter);
    SetLineAlignment(StringAlignmentCenter);
    SetTrimming(StringTrimmingEllipsisCharacter);
    flag := StringFormatFlagsNoWrap or StringFormatFlagsLineLimit;
    SetFormatFlags(flag);
  end;
end;

class procedure TabStyleData.Clear(const AItem: TabStyleData);
begin
  AItem.Frame.free;
  AItem.Background.free;
  AItem.Font.Free;
  AItem.Caption.Free;
  AItem.Format.free;
end;

function TTreeViewSkin.BuildDotBrush: HBRUSH;
const
  LineBitsDotted: array [0..8] of Word = ($55, $AA, $55, $AA, $55, $AA, $55, $AA, $55);
var
  hPatternBitmap: HBITMAP;
  pBits: Pointer;
begin
  ///
  ///  创建点线
  pBits := @LineBitsDotted;
  hPatternBitmap := CreateBitmap(8, 8, 1, 1, pBits);
  Result := CreatePatternBrush(hPatternBitmap);
  DeleteObject(hPatternBitmap);
end;

procedure TTreeViewSkin.DrawFormButton(DC: HDC; AKind: TFormButtonKind; AState:
    TSkinIndicator; const R: TRect);
var
  //rSrcOff: TPoint;
  x, y: integer;
  d: TSkinElementData;
begin
  d := ResAreas[raFormButton];

  /// 绘制背景
  DrawButtonBackground(DC, AState, R);

  /// 绘制图标
  d.X := d.X + d.W * ord(AKind);
  //rSrcOff := Point(d.W * ord(AKind), 0);
  x := R.Left + (R.Right - R.Left - d.W) div 2;
  y := R.Top + (R.Bottom - R.Top - d.H) div 2;
  DrawTransparentBitmap(FData, d.X, d.Y, DC, x, y, d.W, d.H, 255);
end;

class procedure TTreeViewSkin.DrawButtonBackground(DC: HDC; AState:
    TSkinIndicator; const R: TRect; const Opacity: Byte = 255);
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
    else                iColor := SkinColor_Form_background;
    end;
    hB := CreateSolidBrush(iColor);
    FillRect(DC, R, hB);
    DeleteObject(hB);
  end;
end;

class procedure TTreeViewSkin.DrawButtonState(DC: HDC; AState: TSkinIndicator; const R: TRect; const AOpacity: Byte);

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

procedure TTreeViewSkin.DrawElement(E:TSkinScrollBarItem; AKind: TSkinScrollType; DC: HDC; const R: TRect; State: TSkinElemenState);

  procedure OffSetVert(var d: TSkinElementData); inline;
  var
    t : Integer;
  begin
    // 竖性滚动条，的长宽倒置，X方向偏移
    t := d.W;
    d.X := d.X + t * 3;
    d.W := d.H;
    d.H := t;
  end;
var
  hB: HBRUSH;
  d: TSkinElementData;
begin
  case E of
    ssbiBackground:
    begin
      hB := CreateSolidBrush(SkinColor_Scrollbar_Background);
      FillRect(dc, R, hB);
      DeleteObject(hB);
    end;

    ssbiUpBtn,
    ssbiDownBtn:
    begin
      d := ResAreas[raTreeScrollbar];// ELEDATA;
      if AKind = sstVert then
        OffSetVert(d);
      d.X := d.X + d.W * (ord(e) - ord(ssbiUpBtn));
      d.Y := d.Y + d.H * ord(State);

      Draw(DC, d, R);
    end;

    ssbiSlider:
    begin
      d := ResAreas[raTreeScrollbar];
      if r.Width >= d.W then
      begin
        if AKind = sstVert then
          OffSetVert(d);

        d.X := d.X + d.W * (ord(e) - ord(ssbiUpBtn));
        d.Y := d.Y + d.H * ord(State);

        if AKind = sstVert then
          DrawStretchHeight(DC, d, R, Point(5, 5))
        else
          DrawStretchWidth(Dc, d, r, Point(5, 5));
      end;
    end;
    ssbiMark:;
  end;
end;

procedure TTreeViewSkin.DrawElement(DC: HDC; AItem: TSkinToolbarElement; const
    R: TRect; const Opacity: Byte = 255);
var
  rSrc: TSkinElementData;
  x, y: integer;
begin
  rSrc := ResAreas[raFormToolbar];
  rSrc.x :=  rSrc.x + rSrc.w * (ord(AItem) - ord(Low(TSkinToolbarElement)));

  /// 绘制图标
  x := R.Left + (R.Right - R.Left - rSrc.w) div 2;
  y := R.Top + (R.Bottom - R.Top - rSrc.h) div 2;
  DrawTransparentBitmap(FData, rSrc.x, rSrc.y, DC, x, y, rSrc.w, rSrc.h, Opacity);
end;

class procedure TTreeViewSkin.DrawIcon(DC: HDC; R: TRect; ASrc: TBitmap; const
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

initialization
  ElementStyles.Load;


finalization
  if FTreeViewSkin <> nil then
    FTreeViewSkin.Free;
  ElementStyles.Clear;

end.

