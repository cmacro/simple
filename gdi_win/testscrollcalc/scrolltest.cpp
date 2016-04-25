//
// 自定义滚动条测试
// 
// Created by 蘑菇房 moguf.com
//
#include <windows.h>
#include <Winuser.h> // setScrollPos
#include <Windowsx.h> // GET_Y_LPARAM
#include "strsafe.h" // how to scroll text （MyTextWindowProc）


LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);           // 主窗体消息处理
LRESULT CALLBACK scrollWndProc(HWND, UINT, WPARAM, LPARAM);     // 滚动条消息处理

// microsoft demo
LRESULT CALLBACK MyTextWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);


int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   PSTR szCmdLine, int iCmdShow)
{
    static TCHAR szAppName[] = TEXT("scrollcalctest");
    HWND         hwnd;
    MSG          msg;
    WNDCLASS     wndclass;

    wndclass.style         = CS_HREDRAW | CS_VREDRAW;
    wndclass.lpfnWndProc   = WndProc;// MyTextWindowProc; // MyBitmapWindowProc;
    wndclass.cbClsExtra    = 0;
    wndclass.cbWndExtra    = 0;
    wndclass.hInstance     = hInstance;
    wndclass.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
    wndclass.hCursor       = LoadCursor(NULL, IDC_ARROW);
    wndclass.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
    wndclass.lpszMenuName  = NULL;
    wndclass.lpszClassName = szAppName;

    if (!RegisterClass(&wndclass))
        return 0;

    // scroll class
    wndclass.style = 0;
    wndclass.lpfnWndProc   = scrollWndProc;
    wndclass.lpszClassName = TEXT("myscroll");
    wndclass.hbrBackground = 0;
    if (!RegisterClass(&wndclass))
        return 0;

    hwnd = CreateWindow(szAppName, TEXT("测试滚动条"),
                        WS_OVERLAPPEDWINDOW | WS_VSCROLL,
                        600, 400, 350, 250,
                        NULL, NULL, hInstance, NULL);

    ShowWindow(hwnd, iCmdShow);
    UpdateWindow(hwnd);

    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    return (int)msg.wParam;
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    static HWND hScroll;
    static int  rowCount, rowHeight, xClient, yClient;
    HDC         hdc;
    int         i, y, vertPos, printBeg, printEnd;
    PAINTSTRUCT ps;
    SCROLLINFO  si;
    TCHAR       szBuffer[100];
    TEXTMETRIC  tm;
    RECT        r;

    switch (message)
    {
    case WM_CREATE:

        // 测试当前数据行数，用于测试滚动量
        rowCount = 100;

        // 获取一行高度
        hdc = GetDC(hwnd);
        GetTextMetrics(hdc, &tm);
        rowHeight = tm.tmHeight + tm.tmExternalLeading;
        ReleaseDC(hwnd, hdc);

        hScroll = CreateWindow(TEXT("myscroll"), TEXT("myscroll"), WS_CHILD | WS_VISIBLE | BS_OWNERDRAW,
                                  300, 10, 16, 300,
                                  hwnd, 0, ((LPCREATESTRUCT)lParam)->hInstance, NULL);

       
        return 0;

    case WM_SIZE:
        xClient = LOWORD(lParam);
        yClient = HIWORD(lParam);


        // 设置滚动条

        si.cbSize = sizeof(si);
        si.fMask  = SIF_RANGE | SIF_PAGE;
        si.nMin   = 0;
        //
        // 只要超过1行就能滚动，因此需要多加一页的行数。
        si.nMax   = rowCount - 1 + yClient / rowHeight - 1;
        si.nPage  = yClient / rowHeight;
        SetScrollInfo(hwnd, SB_VERT, &si, TRUE);

        // 修正滚动条位置
        MoveWindow(hScroll, xClient - 100, 10 , 16, yClient - 20, true);
        SendMessage(hScroll, SBM_SETSCROLLINFO, TRUE, (LPARAM)(&si));

        return 0;

    case WM_VSCROLL:

        // 获取竖滚动条状态
        si.cbSize = sizeof(si);
        si.fMask  = SIF_ALL;
        GetScrollInfo(hwnd, SB_VERT, &si);
        
        // 保存原来的位置，用于计算滚动当前画布量
        vertPos = si.nPos;

        // lParam 参数为滚动条控件，标准滚动条没有这个参数
        if (lParam) 
            SendMessage(hScroll, SBM_GETSCROLLINFO, NULL, (LPARAM)(&si));

        // 设置滚动量
        switch (LOWORD(wParam))
        {
        case SB_TOP:        si.nPos = si.nMin;      break;
        case SB_BOTTOM:     si.nPos = si.nMax;      break;
        case SB_LINEUP:     si.nPos -= 1;           break;
        case SB_LINEDOWN:   si.nPos += 1;           break;
        case SB_PAGEUP:     si.nPos -= si.nPage;    break;
        case SB_PAGEDOWN:   si.nPos += si.nPage;    break;
        case SB_THUMBTRACK: si.nPos = si.nTrackPos; break;
        default:    break;
        }

        // 更新系统滚动条状态
        si.fMask = SIF_POS;
        SetScrollInfo(hwnd, SB_VERT, &si, TRUE);
        GetScrollInfo(hwnd, SB_VERT, &si);

        // 滚动界面画布
        if (si.nPos != vertPos) {
            GetClientRect(hwnd, &r);
            r.right -= 150; // 不要刷到滚动条位置，否则会闪

            hdc = GetDC(hwnd);
            ScrollDC(hdc, 0, rowHeight * (vertPos - si.nPos), &r, NULL, NULL, NULL);
            ReleaseDC(hwnd, hdc);

            if (rowHeight * (vertPos - si.nPos) < 0 ) r.top = r.bottom + rowHeight * (vertPos - si.nPos);
            else r.bottom = r.top + rowHeight * (vertPos - si.nPos);
            InvalidateRect(hwnd, &r, false);
        }

        // 通知自定义滚动条滚动
        SendMessage(hScroll, SBM_SETSCROLLINFO, TRUE, (LPARAM)(&si));

        return 0;

    case WM_MOUSEWHEEL:
        // 消息发送到滚动条控件
        PostMessage(hScroll, WM_MOUSEWHEEL, wParam, lParam);
        return 0;

    case WM_PAINT:
        hdc = BeginPaint(hwnd, &ps);

        // 获取竖滚动条状态
        si.cbSize = sizeof(si);
        si.fMask  = SIF_ALL;
        GetScrollInfo(hwnd, SB_VERT, &si);
        vertPos = si.nPos;

        // 重绘修改的位置（减少重绘量）
        printBeg = max(0, vertPos + ps.rcPaint.top / rowHeight);
        printEnd = min(rowCount - 1, vertPos + ps.rcPaint.bottom / rowHeight);
        // 输出当前行号
        for (i = printBeg; i <= printEnd; i++)
        {
            y = rowHeight * (i - vertPos);
            TextOut(hdc, 22 , y, szBuffer, wsprintf(szBuffer, TEXT("%5d   "), i+1));
        }

        // 填充空白区域
        y = rowHeight * (printEnd + 1 - vertPos);
        if (ps.rcPaint.bottom - y > 0) {
            r = ps.rcPaint;
            r.top = y;
            ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &r, 0, 0, 0);
        }
        EndPaint(hwnd, &ps);

        return 0;

    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }

    return DefWindowProc(hwnd, message, wParam, lParam);
}

static void mlCGFillColor(HDC hdc, RECT *r, unsigned int color)
{
    // 实际使用ExtTextOut要比FillRect填充颜色效果好，能减少绘制中闪烁问题。
    SetBkColor(hdc, color);
    ExtTextOut(hdc, 0, 0, ETO_OPAQUE, r, 0, 0, 0);
}

// 计算滚动条位置
static int calcVertThumPos(int postion, SCROLLINFO *si, int s)
{
    float thumsize;     // 滑块的大小
    float pxSize;       // 每象素可滑动量
    int   scrollCnt;    // 可滑动数量

    // 滑块尺寸 =  滚动条高度 / 有效范围 * 每页数量
    //            最小 20
    thumsize = max((float)((s) / (si->nMax - si->nMin) * si->nPage), 20.0f);
    if (postion <= (int)(thumsize / 2))
        return 0;
    if (postion >= s - (thumsize / 2))
        return (si->nMax - si->nMin + 1) - si->nPage;

    // 计算方法：
    //  每象素滑动量 = 有效高度 / 可滑动数量
    //  有效高度     = 总高度 - 滑块尺寸
    //  可滑动数量   = 总量 - 每页数量
    scrollCnt = (si->nMax - si->nMin + 1) - si->nPage;
    pxSize = (float)(s - thumsize) / (float)scrollCnt;

    //
    // 计算方法：
    //   位置 = （鼠标位置 - 半滑块尺寸） / 每象素滑动量
    return (int)((postion - thumsize / 2.0) / pxSize);
}

LRESULT CALLBACK scrollWndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    HDC hdc;
    PAINTSTRUCT ps;
    SCROLLINFO *psrcsi;
    RECT r;

    TCHAR       szBuffer[100];

    float s;                // 滑块的尺寸
    int v;                  // 滑块界面Top位置
    static SCROLLINFO  si;  // 用于保存滚动条信息
    static int dragState;   // 拖拽状态
    static int height;      // 滚动区域高度

    int accumDelta;

    //
    // 滚动条设置消息
    //  SBM_SETSCROLLINFO SBM_GETSCROLLINFO 消息参数
    //  wParam --- 是否要刷新 (SBM_GETSCROLLINFO 无用)
    //  lParam --- *SCROLLINFO
    switch (message) {
    case SBM_SETSCROLLINFO:
        if (!lParam)
            return 0;

        // 设置滚动条信息
        psrcsi = (SCROLLINFO *)lParam;
        if (psrcsi->fMask & SIF_RANGE) {    // 设置内容行数
            si.nMax = psrcsi->nMax;
            si.nMin = psrcsi->nMin;
        }
        if (psrcsi->fMask & SIF_PAGE)       // 每页能显示多少行
            si.nPage = psrcsi->nPage;
        if (psrcsi->fMask & SIF_POS)        // 行显示位置
            si.nPos = psrcsi->nPos;

        wsprintf(szBuffer, TEXT("yPos:%3d "), si.nPos);
        SetWindowText(GetParent(hwnd), szBuffer);

        // wParam = true　刷新滚动区域
        if (wParam) 
            InvalidateRect(hwnd, NULL, false);
        return 0;

    case SBM_GETSCROLLINFO:
        //这里简单处理，应该和SBM_SETSCROLLINFO一样判断获取的信息。
        if (lParam)
            *(SCROLLINFO *)lParam = si;     
        return 0;

    case WM_SIZE:
        height = HIWORD(lParam);
        break;

    case WM_LBUTTONDOWN:
        si.nTrackPos = calcVertThumPos(GET_Y_LPARAM(lParam), &si, height);
        if (si.nPos != si.nTrackPos) 
            PostMessage(GetParent(hwnd), WM_VSCROLL, SB_THUMBTRACK, (LPARAM)hwnd);
        dragState = 1; // 准备拖动滚动条
        InvalidateRect(hwnd, NULL, false);
        return 0;
    
    case WM_MOUSEMOVE:
        if (dragState == 1) {
            // 有拖拽准备，锁定鼠标鼠标
            SetCapture(hwnd);
            dragState = 2;
            
        }
        else if (dragState == 2) {
            if (!(wParam & MK_LBUTTON)) {
                dragState = 0;              // 防止中间中断，导致出现无效拖拽
                if (GetCapture() == hwnd)
                    ReleaseCapture();
            }
            else {
                // 在锁定状态下拖动滚动条定位。
                si.nTrackPos = calcVertThumPos(GET_Y_LPARAM(lParam), &si, height);
                if (si.nTrackPos != si.nPos)
                    PostMessage(GetParent(hwnd), WM_VSCROLL, SB_THUMBTRACK, (LPARAM)hwnd);
            }
        }
        return 0;

    case WM_LBUTTONUP:
        if (dragState == 2) 
            ReleaseCapture();   // 释放鼠标锁定

        if (dragState) {
            dragState = 0;          // 清除状态
            InvalidateRect(hwnd, NULL, false);
        }
        return 0;

    case WM_MOUSEWHEEL:
        // 鼠标滚动支持
        accumDelta = GET_WHEEL_DELTA_WPARAM(wParam) / WHEEL_DELTA;  // 滚动精度120一个单位
        si.nTrackPos = si.nPos - accumDelta * 3;   // 每滚一次3行
        if (si.nTrackPos < 0)
            si.nTrackPos = 0;
        else if (si.nTrackPos > (int)((si.nMax - si.nMin + 1) - si.nPage))
            si.nTrackPos = (si.nMax - si.nMin + 1) - si.nPage;

        if (si.nPos != si.nTrackPos)
            PostMessage(GetParent(hwnd), WM_VSCROLL, SB_THUMBTRACK, (LPARAM)hwnd);
            
        return 0;


    case WM_PAINT:
        hdc = BeginPaint(hwnd, &ps);

        // 绘制背景色
        GetClientRect(hwnd, &r);
        mlCGFillColor(hdc, &r, 0xcccccc);

        // 计算滑块大小，过小时不绘制
        if (r.bottom - r.top > 30 && si.nMax && (si.nMax - si.nMin) >= (int)si.nPage) {

            // 滑块计算
            //   大小 = 滚动条高度 / 有效范围 * 每页数量
            //   最小20, 内容比较多，降低用于鼠标定位到滚动条拖动的难度。
            s = max((float)((r.bottom - r.top) / (si.nMax - si.nMin) * si.nPage), 20.0f);

            // 实际滑动位置
            //  = （滚动条高度 - 滑块尺寸） / （有效范围 - 每页数量 + 1） * 当前行位置
            // 实际滚动的位置会比实际少一页的数量。
            //
            v = 0;
            if (si.nPos > 0) 
                v = (int)((r.bottom - r.top - s) / (float)(si.nMax - si.nMin + 1 - si.nPage)  * si.nPos);
            // 由于精度问题，可能滑块位置会超界。超界就取最大值
            if (v && v + (int)s > r.bottom) 
                v = r.bottom - (int)s;

            // 绘制滑块
            r.left++;
            r.right--;
            r.top = v ;
            r.bottom = r.top + (int)s;
            
            // 拖拽时滑块颜色反一下
            mlCGFillColor(hdc, &r, dragState ? 0x999999 : 0x666666e);
            InflateRect(&r, -1, -1);
            mlCGFillColor(hdc, &r, dragState ? 0x666666e : 0x999999);
        }

        EndPaint(hwnd, &ps);

        return 0;
    }
    return DefWindowProc(hwnd, message, wParam, lParam);
}



LRESULT CALLBACK MyTextWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    //
    // How to Scroll Text
    // https://msdn.microsoft.com/en-us/library/windows/desktop/hh298421(v=vs.85).aspx
    // 

    HDC hdc;
    PAINTSTRUCT ps;
    TEXTMETRIC tm;
    SCROLLINFO si;

    // These variables are required to display text. 
    static int xClient;     // width of client area 
    static int yClient;     // height of client area 
    static int xClientMax;  // maximum width of client area 

    static int xChar;       // horizontal scrolling unit 
    static int yChar;       // vertical scrolling unit 
    static int xUpper;      // average width of uppercase letters 

    static int xPos;        // current horizontal scrolling position 
    static int yPos;        // current vertical scrolling position 

    int i;                  // loop counter 
    int x, y;               // horizontal and vertical coordinates

    int FirstLine;          // first line in the invalidated area 
    int LastLine;           // last line in the invalidated area 
    HRESULT hr;
    size_t abcLength;        // length of an abc[] item 

                             // Create an array of lines to display. 
#define LINES 28 
    static TCHAR *abc[] ={
        TEXT("anteater"),  TEXT("bear"),      TEXT("cougar"),
        TEXT("dingo"),     TEXT("elephant"),  TEXT("falcon"),
        TEXT("gazelle"),   TEXT("hyena"),     TEXT("iguana"),
        TEXT("jackal"),    TEXT("kangaroo"),  TEXT("llama"),
        TEXT("moose"),     TEXT("newt"),      TEXT("octopus"),
        TEXT("penguin"),   TEXT("quail"),     TEXT("rat"),
        TEXT("squid"),     TEXT("tortoise"),  TEXT("urus"),
        TEXT("vole"),      TEXT("walrus"),    TEXT("xylophone"),
        TEXT("yak"),       TEXT("zebra"),
        TEXT("This line contains words, but no character. Go figure."),
        TEXT("")
    };

    switch (uMsg)
    {
    case WM_CREATE:
        // Get the handle to the client area's device context. 
        hdc = GetDC(hwnd);

        // Extract font dimensions from the text metrics. 
        GetTextMetrics(hdc, &tm);
        xChar = tm.tmAveCharWidth;
        xUpper = (tm.tmPitchAndFamily & 1 ? 3 : 2) * xChar / 2;
        yChar = tm.tmHeight + tm.tmExternalLeading;

        // Free the device context. 
        ReleaseDC(hwnd, hdc);

        // Set an arbitrary maximum width for client area. 
        // (xClientMax is the sum of the widths of 48 average 
        // lowercase letters and 12 uppercase letters.) 
        xClientMax = 48 * xChar + 12 * xUpper;

        return 0;

    case WM_SIZE:

        // Retrieve the dimensions of the client area. 
        yClient = HIWORD(lParam);
        xClient = LOWORD(lParam);

        // Set the vertical scrolling range and page size
        si.cbSize = sizeof(si);
        si.fMask  = SIF_RANGE | SIF_PAGE;
        si.nMin   = 0;
        si.nMax   = LINES - 1;
        si.nPage  = yClient / yChar;
        SetScrollInfo(hwnd, SB_VERT, &si, TRUE);

        // Set the horizontal scrolling range and page size. 
        si.cbSize = sizeof(si);
        si.fMask  = SIF_RANGE | SIF_PAGE;
        si.nMin   = 0;
        si.nMax   = 2 + xClientMax / xChar;
        si.nPage  = xClient / xChar;
        SetScrollInfo(hwnd, SB_HORZ, &si, TRUE);

        return 0;
    case WM_HSCROLL:
        // Get all the vertial scroll bar information.
        si.cbSize = sizeof(si);
        si.fMask  = SIF_ALL;

        // Save the position for comparison later on.
        GetScrollInfo(hwnd, SB_HORZ, &si);
        xPos = si.nPos;
        switch (LOWORD(wParam))
        {
            // User clicked the left arrow.
        case SB_LINELEFT:
            si.nPos -= 1;
            break;

            // User clicked the right arrow.
        case SB_LINERIGHT:
            si.nPos += 1;
            break;

            // User clicked the scroll bar shaft left of the scroll box.
        case SB_PAGELEFT:
            si.nPos -= si.nPage;
            break;

            // User clicked the scroll bar shaft right of the scroll box.
        case SB_PAGERIGHT:
            si.nPos += si.nPage;
            break;

            // User dragged the scroll box.
        case SB_THUMBTRACK:
            si.nPos = si.nTrackPos;
            break;

        default:
            break;
        }

        // Set the position and then retrieve it.  Due to adjustments
        // by Windows it may not be the same as the value set.
        si.fMask = SIF_POS;
        SetScrollInfo(hwnd, SB_HORZ, &si, TRUE);
        GetScrollInfo(hwnd, SB_HORZ, &si);

        // If the position has changed, scroll the window.
        if (si.nPos != xPos)
        {
            ScrollWindow(hwnd, xChar * (xPos - si.nPos), 0, NULL, NULL);
        }

        return 0;

    case WM_VSCROLL:
        // Get all the vertial scroll bar information.
        si.cbSize = sizeof(si);
        si.fMask  = SIF_ALL;
        GetScrollInfo(hwnd, SB_VERT, &si);

        // Save the position for comparison later on.
        yPos = si.nPos;
        switch (LOWORD(wParam))
        {

            // User clicked the HOME keyboard key.
        case SB_TOP:
            si.nPos = si.nMin;
            break;

            // User clicked the END keyboard key.
        case SB_BOTTOM:
            si.nPos = si.nMax;
            break;

            // User clicked the top arrow.
        case SB_LINEUP:
            si.nPos -= 1;
            break;

            // User clicked the bottom arrow.
        case SB_LINEDOWN:
            si.nPos += 1;
            break;

            // User clicked the scroll bar shaft above the scroll box.
        case SB_PAGEUP:
            si.nPos -= si.nPage;
            break;

            // User clicked the scroll bar shaft below the scroll box.
        case SB_PAGEDOWN:
            si.nPos += si.nPage;
            break;

            // User dragged the scroll box.
        case SB_THUMBTRACK:
            si.nPos = si.nTrackPos;
            break;

        default:
            break;
        }

        // Set the position and then retrieve it.  Due to adjustments
        // by Windows it may not be the same as the value set.
        si.fMask = SIF_POS;
        SetScrollInfo(hwnd, SB_VERT, &si, TRUE);
        GetScrollInfo(hwnd, SB_VERT, &si);

        // If the position has changed, scroll window and update it.
        if (si.nPos != yPos)
        {
            ScrollWindow(hwnd, 0, yChar * (yPos - si.nPos), NULL, NULL);
            UpdateWindow(hwnd);
        }

        return 0;

    case WM_PAINT:
        // Prepare the window for painting.
        hdc = BeginPaint(hwnd, &ps);

        // Get vertical scroll bar position.
        si.cbSize = sizeof(si);
        si.fMask  = SIF_POS;
        GetScrollInfo(hwnd, SB_VERT, &si);
        yPos = si.nPos;

        // Get horizontal scroll bar position.
        GetScrollInfo(hwnd, SB_HORZ, &si);
        xPos = si.nPos;

        // Find painting limits.
        FirstLine = max(0, yPos + ps.rcPaint.top / yChar);
        LastLine = min(LINES - 1, yPos + ps.rcPaint.bottom / yChar);

        for (i = FirstLine; i <= LastLine; i++)
        {
            x = xChar * (1 - xPos);
            y = yChar * (i - yPos);

            // Note that "55" in the following depends on the 
            // maximum size of an abc[] item. Also, you must include
            // strsafe.h to use the StringCchLength function.
            hr = StringCchLength(abc[i], 55, &abcLength);
            if ((FAILED(hr)) | (abcLength == NULL))
            {
                //
                // TODO: write error handler
                //
            }

            // Write a line of text to the client area.
            TextOut(hdc, x, y, abc[i], (int)abcLength);
        }

        // Indicate that painting is finished.
        EndPaint(hwnd, &ps);
        return 0;

    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }

    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}