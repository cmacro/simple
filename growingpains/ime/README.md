# Ime test

输入法控制测试。为解决输入法在实际编辑中光标跟随问题。

控制消息 WM_IME_XXX

:::c
```
    if (uMsg == WM_IME_STARTCOMPOSITION)
    {
      
        COMPOSITIONFORM cf;
        LOGFONTW lfW;
        HIMC hIMC;

        if (hIMC=ImmGetContext(ae->hWndEdit))
        {
          cf.dwStyle=CFS_POINT;
          cf.ptCurrentPos.x=ae->rcDraw.left + ae->ptCaret.x;
          cf.ptCurrentPos.y=ae->rcDraw.top + ae->ptCaret.y;
          ImmSetCompositionWindow(hIMC, &cf);

          GetObjectW(ae->hFont, sizeof(LOGFONTW), &lfW);
          ImmSetCompositionFont(hIMC, &lfW);

          ImmReleaseContext(ae->hWndEdit, hIMC);
        }
      
    }
```


## imetest 
简易测试输入法



## akeleditimetest
使用AkelEdit控件测试输入法