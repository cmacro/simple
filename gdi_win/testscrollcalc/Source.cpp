
#include <windows.h>

LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK scrollWndProc(HWND, UINT, WPARAM, LPARAM);


int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   PSTR szCmdLine, int iCmdShow)
{
    static TCHAR szAppName[] = TEXT("scrollcalctest");
    HWND         hwnd;
    MSG          msg;
    WNDCLASS     wndclass;

    wndclass.style         = CS_HREDRAW | CS_VREDRAW;
    wndclass.lpfnWndProc   = WndProc;
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
    wndclass.lpfnWndProc   = scrollWndProc;
    wndclass.lpszClassName = TEXT("myscroll");
    wndclass.hbrBackground = (HBRUSH)GetStockObject(GRAY_BRUSH);
    if (!RegisterClass(&wndclass))
        return 0;


    hwnd = CreateWindow(szAppName, TEXT("测试滚动条"),
                        WS_OVERLAPPEDWINDOW | WS_VSCROLL,
                        200, 200,
                        600, 500,
                        NULL, NULL, hInstance, NULL);

    ShowWindow(hwnd, iCmdShow);
    UpdateWindow(hwnd);

    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    return msg.wParam;
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    static HWND hwndScroll;
    static int  cxChar, cxCaps, cyChar, cxClient, cyClient;
    HDC         hdc;
    int         i, x, y, iVertPos, iPaintBeg, iPaintEnd;
    PAINTSTRUCT ps;
    SCROLLINFO  si;
    TCHAR       szBuffer[100];
    TEXTMETRIC  tm;
    static      int rowCount;

    switch (message)
    {
    case WM_CREATE:

        hdc = GetDC(hwnd);

        // 测试当前数据行数，用于测试滚动量
        rowCount = 100000;  

        GetTextMetrics(hdc, &tm);
        cxChar = tm.tmAveCharWidth;
        cxCaps = (tm.tmPitchAndFamily & 1 ? 3 : 2) * cxChar / 2;
        cyChar = tm.tmHeight + tm.tmExternalLeading;
        ReleaseDC(hwnd, hdc);

        hwndScroll = CreateWindow(TEXT("myscroll"), TEXT("myscroll"), WS_CHILD | WS_VISIBLE | BS_OWNERDRAW,
                                  350, 10, 20, 480,
                                  hwnd, 0, ((LPCREATESTRUCT)lParam)->hInstance, NULL);

       
        return 0;

    case WM_SIZE:
        cxClient = LOWORD(lParam);
        cyClient = HIWORD(lParam);

        MoveWindow(hwndScroll, 350, 10, 20, cyClient - 20, true);

        // 设置滚动条

        si.cbSize = sizeof(si);
        si.fMask  = SIF_RANGE | SIF_PAGE;
        si.nMin   = 0;
        si.nMax   = rowCount - 1 + cyClient / cyChar - 1;
        si.nPage  = cyClient / cyChar;
        SetScrollInfo(hwnd, SB_VERT, &si, TRUE);

        return 0;

    case WM_VSCROLL:
        // 获取竖滚动条状态
        si.cbSize = sizeof(si);
        si.fMask  = SIF_ALL;
        GetScrollInfo(hwnd, SB_VERT, &si);

        // 保存原来的位置，用于计算滚动当前画布量
        iVertPos = si.nPos;

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
        if (si.nPos != iVertPos) {
            ScrollWindow(hwnd, 0, cyChar * (iVertPos - si.nPos), NULL, NULL);
            UpdateWindow(hwnd);
        }

        MoveWindow(hwndScroll, 350, 10, 20, cyClient - 20, true);

        return 0;

    case WM_PAINT:
        hdc = BeginPaint(hwnd, &ps);

        // 获取竖滚动条状态
        si.cbSize = sizeof(si);
        si.fMask  = SIF_ALL;
        GetScrollInfo(hwnd, SB_VERT, &si);
        iVertPos = si.nPos;

        // 重绘修改的位置（减少重绘量）
        iPaintBeg = max(0, iVertPos + ps.rcPaint.top / cyChar);
        iPaintEnd = min(rowCount - 1, iVertPos + ps.rcPaint.bottom / cyChar);

        SetTextAlign(hdc, TA_LEFT);
        for (i = iPaintBeg; i <= iPaintEnd; i++)
        {
            y = cyChar * (i - iVertPos);
            TextOut(hdc, 22 , y, szBuffer, wsprintf(szBuffer, TEXT("%5d"), i));
        }

        EndPaint(hwnd, &ps);

        wsprintf(szBuffer, TEXT("nPos:%5d    nTrackPos:%5d"), si.nPos, si.nTrackPos);
        SetWindowText(hwnd, szBuffer);


        return 0;

    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }

    return DefWindowProc(hwnd, message, wParam, lParam);
}


LRESULT CALLBACK scrollWndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    static SCROLLINFO  si;

    switch (message)
    {
    default:
        break;

    }

    return DefWindowProc(hwnd, message, wParam, lParam);
}
