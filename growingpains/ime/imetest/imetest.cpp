// imetest.cpp : Defines the entry point for the application.
//

#include "stdafx.h"
#include "imetest.h"

#define MAX_LOADSTRING 100

// Global Variables:
HINSTANCE hInst;								// current instance
TCHAR szTitle[MAX_LOADSTRING];					// The title bar text
TCHAR szWindowClass[MAX_LOADSTRING];			// the main window class name

// Forward declarations of functions included in this code module:
ATOM				MyRegisterClass(HINSTANCE hInstance);
BOOL				InitInstance(HINSTANCE, int);
LRESULT CALLBACK	WndProc(HWND, UINT, WPARAM, LPARAM);
INT_PTR CALLBACK	About(HWND, UINT, WPARAM, LPARAM);

int APIENTRY _tWinMain(_In_ HINSTANCE hInstance,
    _In_opt_ HINSTANCE hPrevInstance,
    _In_ LPTSTR    lpCmdLine,
    _In_ int       nCmdShow)
{
    UNREFERENCED_PARAMETER(hPrevInstance);
    UNREFERENCED_PARAMETER(lpCmdLine);

    // TODO: Place code here.
    MSG msg;
    HACCEL hAccelTable;

    // Initialize global strings
    LoadString(hInstance, IDS_APP_TITLE, szTitle, MAX_LOADSTRING);
    LoadString(hInstance, IDC_IMETEST, szWindowClass, MAX_LOADSTRING);
    MyRegisterClass(hInstance);

    // Perform application initialization:
    if (!InitInstance(hInstance, nCmdShow))
    {
        return FALSE;
    }

    hAccelTable = LoadAccelerators(hInstance, MAKEINTRESOURCE(IDC_IMETEST));

    // Main message loop:
    while (GetMessage(&msg, NULL, 0, 0))
    {
        if (!TranslateAccelerator(msg.hwnd, hAccelTable, &msg))
        {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }

    return (int)msg.wParam;
}

//
//  FUNCTION: MyRegisterClass()
//
//  PURPOSE: Registers the window class.
//
ATOM MyRegisterClass(HINSTANCE hInstance)
{
    WNDCLASSEX wcex;

    wcex.cbSize = sizeof(WNDCLASSEX);

    wcex.style = CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc = WndProc;
    wcex.cbClsExtra = 0;
    wcex.cbWndExtra = 0;
    wcex.hInstance = hInstance;
    wcex.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_IMETEST));
    wcex.hCursor = LoadCursor(NULL, IDC_ARROW);
    wcex.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
    wcex.lpszMenuName = MAKEINTRESOURCE(IDC_IMETEST);
    wcex.lpszClassName = szWindowClass;
    wcex.hIconSm = LoadIcon(wcex.hInstance, MAKEINTRESOURCE(IDI_SMALL));

    return RegisterClassEx(&wcex);
}

//
//   FUNCTION: InitInstance(HINSTANCE, int)
//
//   PURPOSE: Saves instance handle and creates main window
//
//   COMMENTS:
//
//        In this function, we save the instance handle in a global variable and
//        create and display the main program window.
//
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
    HWND hWnd;

    hInst = hInstance; // Store instance handle in our global variable

    hWnd = CreateWindow(szWindowClass, szTitle, WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, NULL, NULL, hInstance, NULL);

    if (!hWnd)
    {
        return FALSE;
    }

    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd);

    return TRUE;
}

//
//  FUNCTION: WndProc(HWND, UINT, WPARAM, LPARAM)
//
//  PURPOSE:  Processes messages for the main window.
//
//  WM_COMMAND	- process the application menu
//  WM_PAINT	- Paint the main window
//  WM_DESTROY	- post a quit message and return
//
//
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    PAINTSTRUCT ps;
    HDC hdc;
    RECT r;
    SIZE sz;
    COLORREF saveColor;
    static TCHAR buffer[2048];
    static int textlen = 0;
    static int textwidth = 0;

    static HFONT hFont = 0;


    switch (message)
    {
    case WM_CREATE:
        hFont = CreateFont(17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, L"微软雅黑");
        return 0;

    case WM_PAINT:
        hdc = BeginPaint(hWnd, &ps);

        // 设置个可观察位置
        SetRect(&r, 93, 100, 95, 120);
        saveColor = SetBkColor(hdc, 0x00cccccc);
        ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &r, NULL, 0, NULL);
        SetBkColor(hdc, saveColor);

        // 清除绘制的字符串
        SetRect(&r, 100, 100, 100 + textwidth + 100, 300);
        ExtTextOut(hdc, 100, 100, ETO_OPAQUE, &r, NULL, 0, NULL);

        // 绘制字符串
        if (textlen) 
        {
            SelectObject(hdc, (HFONT)hFont);
            ExtTextOut(hdc, 100, 100, ETO_OPAQUE, NULL, buffer, textlen, NULL);
        }

        EndPaint(hWnd, &ps);
        return 0;

    case WM_SETFOCUS:
        CreateCaret(hWnd, NULL, 1, 20);
        SetCaretPos(100 + textwidth, 100);
        ShowCaret(hWnd);
        return 0;

    case WM_KILLFOCUS:
        HideCaret(hWnd);
        DestroyCaret();
        return 0;

    case WM_CHAR:
        if (wParam >= 0x20 && wParam != 0x7F)
        {
            buffer[textlen] = wParam;
            textlen++;
        }
        else if (wParam == VK_BACK)
        {
            if (textlen) textlen--;
        }
        else
            break;

        // 获取字体宽度，
        //   用来确定Caret的位置
        if (hdc = GetDC(hWnd))
        {
            SelectObject(hdc, (HFONT)hFont);
            GetTextExtentPoint32(hdc, buffer, textlen, &sz);
            textwidth = sz.cx;
            ReleaseDC(hWnd, hdc);
        }
        SetCaretPos(100 + textwidth, 100);

        InvalidateRect(hWnd, NULL, FALSE);
        return 1;

    case WM_IME_STARTCOMPOSITION:
    {
        LOGFONT lf;
        COMPOSITIONFORM cf;
        HIMC himc = ImmGetContext(hWnd);
        if (himc)
        {
            // 设置输入法显示位置。
            cf.dwStyle = CFS_POINT;
            cf.ptCurrentPos.y = 100;
            cf.ptCurrentPos.x = 100 + textwidth;
            ImmSetCompositionWindow(himc, &cf);

            //输入法字体样式
            GetObject(hFont, sizeof(LOGFONT), &lf);
            ImmSetCompositionFont(himc, &lf);

            ImmReleaseContext(hWnd, himc);
        }
    }
    //return 1;  // 这个消息一定要让系统继续处理，否则在内嵌输入法是无法显示输入的拼音内容。
    break;

    case WM_DESTROY:
        DeleteObject(hFont);
        PostQuitMessage(0);
        return 0;
    }

    return DefWindowProc(hWnd, message, wParam, lParam);
}

// Message handler for about box.
INT_PTR CALLBACK About(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
    UNREFERENCED_PARAMETER(lParam);
    switch (message)
    {
    case WM_INITDIALOG:
        return (INT_PTR)TRUE;

    case WM_COMMAND:
        if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)
        {
            EndDialog(hDlg, LOWORD(wParam));
            return (INT_PTR)TRUE;
        }
        break;
    }
    return (INT_PTR)FALSE;
}
