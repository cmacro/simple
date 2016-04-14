
#include <windows.h>

LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);

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

    hwnd = CreateWindow(szAppName, TEXT("测试滚动条"),
                        WS_OVERLAPPEDWINDOW | WS_VSCROLL | WS_HSCROLL,
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
    static int  cxChar, cxCaps, cyChar, cxClient, cyClient, iMaxWidth;
    HDC         hdc;
    int         i, x, y, iVertPos, iHorzPos, iPaintBeg, iPaintEnd;
    PAINTSTRUCT ps;
    SCROLLINFO  si;
    TCHAR       szBuffer[10];
    TEXTMETRIC  tm;
    static      int rowCount;

    switch (message)
    {
    case WM_CREATE:
        hdc = GetDC(hwnd);

        rowCount = 10;

        GetTextMetrics(hdc, &tm);
        cxChar = tm.tmAveCharWidth;
        cxCaps = (tm.tmPitchAndFamily & 1 ? 3 : 2) * cxChar / 2;
        cyChar = tm.tmHeight + tm.tmExternalLeading;
        ReleaseDC(hwnd, hdc);
        iMaxWidth = 40 * cxChar + 22 * cxCaps;
        return 0;

    case WM_SIZE:
        cxClient = LOWORD(lParam);
        cyClient = HIWORD(lParam);

        // 设置滚动条

        si.cbSize = sizeof(si);
        si.fMask  = SIF_RANGE | SIF_PAGE;
        si.nMin   = 0;
        si.nMax   = rowCount - 1 + cyClient / cyChar - 1;
        si.nPage  = cyClient / cyChar;
        SetScrollInfo(hwnd, SB_VERT, &si, TRUE);

        return 0;

    case WM_VSCROLL:
        // Get all the vertial scroll bar information
        si.cbSize = sizeof(si);
        si.fMask  = SIF_ALL;
        GetScrollInfo(hwnd, SB_VERT, &si);

        // Save the position for comparison later on
        iVertPos = si.nPos;
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
        // Set the position and then retrieve it.  Due to adjustments
        //   by Windows it may not be the same as the value set.

        si.fMask = SIF_POS;
        SetScrollInfo(hwnd, SB_VERT, &si, TRUE);
        GetScrollInfo(hwnd, SB_VERT, &si);

        // If the position has changed, scroll the window and update it
        if (si.nPos != iVertPos) {
            ScrollWindow(hwnd, 0, cyChar * (iVertPos - si.nPos), NULL, NULL);
            UpdateWindow(hwnd);
        }
        return 0;

    case WM_PAINT:
        hdc = BeginPaint(hwnd, &ps);

        // Get vertical scroll bar position
        si.cbSize = sizeof(si);
        si.fMask  = SIF_POS;
        GetScrollInfo(hwnd, SB_VERT, &si);
        iVertPos = si.nPos;

        // Get horizontal scroll bar position
        GetScrollInfo(hwnd, SB_HORZ, &si);
        iHorzPos = si.nPos;

        // Find painting limits
        iPaintBeg = max(0, iVertPos + ps.rcPaint.top / cyChar);
        iPaintEnd = min(rowCount - 1, iVertPos + ps.rcPaint.bottom / cyChar);

        SetTextAlign(hdc, TA_LEFT);
        for (i = iPaintBeg; i <= iPaintEnd; i++)
        {
            x = cxChar * (1 - iHorzPos);
            y = cyChar * (i - iVertPos);

            TextOut(hdc, 22 , y, szBuffer, 
                    wsprintf(szBuffer, TEXT("%5d"), si.nPos + i));
        }

        EndPaint(hwnd, &ps);
        return 0;

    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }

    return DefWindowProc(hwnd, message, wParam, lParam);
}
