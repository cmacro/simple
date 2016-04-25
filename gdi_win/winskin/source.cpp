//
// 窗体皮肤实现 - 在VC中简单实现绘制（五）
// 
// blog http://www.moguf.com/post/devwinskin05
//
// Created by 蘑菇房 moguf.com
//
#include <windows.h>

LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);           // 主窗体消息处理

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   PSTR szCmdLine, int iCmdShow)
{
    static TCHAR szAppName[] = TEXT("winskin5");
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

    hwnd = CreateWindow(szAppName, TEXT("窗体皮肤实现 - 在VC中简单实现绘制（五）"),
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
    HDC hdc;

    RECT rw;            // 窗体大小，实际需要绘制非客户区的尺寸
    RECT rc;            // 客户区大小，用于绘制非客户区时扣除
    POINT pt;           //
    HRGN hTmp;          // 用于重新生成倒角的窗体区域
    BOOL bChanged;      //

    static HRGN hRegion;            // 窗体样式句柄
    static BOOL bChangeSizeCalled;  // 窗体修改状态
    static RECT rWindowSize;        // 窗体尺寸状态


    switch (message) {
    case WM_CREATE:
        hRegion = 0;
        bChangeSizeCalled = FALSE;
        rWindowSize = { 0,0,0,0 };
        break;

    case WM_NCPAINT:
        GetWindowRect(hwnd, &rw);
        GetClientRect(hwnd, &rc);
        pt.x = rc.left;
        pt.y = rc.top;
        ClientToScreen(hwnd, &pt);
        OffsetRect(&rc, pt.x - rw.left, pt.y - rw.top);

        hdc = GetWindowDC(hwnd);
        ExcludeClipRect(hdc, rc.left, rc.top, rc.right, rc.bottom);

        OffsetRect(&rw, -rw.left, -rw.top);
        // 使用这个方式比使用fillrect函数填充效果好，不闪烁
        SetBkColor(hdc, 0xBF7B18);
        ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rw, 0, 0, 0);

        ReleaseDC(hwnd, hdc);
        return 0;

    case WM_NCCALCSIZE:
        // 设置窗体非客户区尺寸
        ((LPNCCALCSIZE_PARAMS)lParam)->rgrc[0].left += 3;
        ((LPNCCALCSIZE_PARAMS)lParam)->rgrc[0].top += 55;
        ((LPNCCALCSIZE_PARAMS)lParam)->rgrc[0].right -= 4;
        ((LPNCCALCSIZE_PARAMS)lParam)->rgrc[0].bottom -= 4;
        return 0;

    case WM_NCACTIVATE:
        // 请求冲刷非客户区
        PostMessage(hwnd, WM_NCPAINT, 1, 0);
        return 0;

    case WM_WINDOWPOSCHANGING:
        bChanged = FALSE;

        // 窗体位置发生改变，重新计算绘制样式
        if (!bChangeSizeCalled) {
            bChanged = (((LPWINDOWPOS)lParam)->flags & SWP_FRAMECHANGED);

            if ((((LPWINDOWPOS)lParam)->flags & SWP_NOMOVE) == 0) {
                rWindowSize.left = ((LPWINDOWPOS)lParam)->x;
                rWindowSize.top = ((LPWINDOWPOS)lParam)->y;
            }
            if ((((LPWINDOWPOS)lParam)->flags & SWP_NOSIZE) == 0) {
                bChanged = bChanged || (((LPWINDOWPOS)lParam)->cx != rWindowSize.right) || (((LPWINDOWPOS)lParam)->cy != rWindowSize.bottom);
                rWindowSize.right = ((LPWINDOWPOS)lParam)->cx;
                rWindowSize.bottom = ((LPWINDOWPOS)lParam)->cy;
            }

            bChanged = bChanged && ((rWindowSize.right * rWindowSize.bottom) != 0);

            if (bChanged) {
                bChangeSizeCalled = TRUE;
                __try {
                    hTmp = hRegion;
                    hRegion = CreateRoundRectRgn(0, 0, rWindowSize.right, rWindowSize.bottom, 3, 3);
                    SetWindowRgn(hwnd, hRegion, TRUE);
                    if (hTmp)
                        DeleteObject(hTmp);
                } __finally {
                    bChangeSizeCalled = FALSE;
                }
            }
        }

        if (!bChanged)
            return DefWindowProc(hwnd, message, wParam, lParam);

        return 0;

    case WM_DESTROY:
        if (hRegion) {
            DeleteObject(hRegion);
            hRegion = 0;
        }
        PostQuitMessage(0);
        return 0;
    }

    return DefWindowProc(hwnd, message, wParam, lParam);
}