对于Edit 来说 
SCROLLINFO si1; 
si1.nMin:0, si1.nMax:14, si1.nPage:6, si1.nPos:9, si1.nTrackPos:9
 
例如上面信息：
si1.nMax - si1.nMin + 1
代表编辑框中文字的总行数 
si1.nPage:6
编辑框一页可以容纳的行数(在不足一显示页时为当前页中的行数)，
一页：在不用滚动条的情况下最多可以显示的行数。编辑框大，可以显示nPage的就大
 si1.nPos:9， si1.nTrackPos:9
滚动条把Edit中文本滚过的行数， 每次点击滚动条上下箭头，编辑框滚动1行
 
假设 nPage=10,
si1.nMax - si1.nMin + 1 = 100
则nPos最大为 100 - 10 = 90, 对于垂直滚动条来说，此时滑块滚到了最底端。
 
垂直滚动条滑块高度的计算：
si1.nMax - si1.nMin + 1 = 800   编辑框文本总行数
si1.nPage = 100                       编辑框最多一页最多容纳 100 行
 
假设垂直滚动条：
TotalHeight = 100 像素, 上下2个Arrow各 10像素，
则 InterHeight = TotalHeight - HeightOf(Arrow)*2 = 100 -10*2 = 80
 
ThumbHeight = si1.nPage * InterHeight /(si1.nMax - si1.nMin + 1) 
                        = 100 * 80/(800) = 10 像素
 
计算滑块的位置：
对于垂直滚动条，计算滑块Thumb的最上端的位置 ThumbPos
EmptyHeight = InterHeight – ThumbHeight
EmptyHeight: 滚动条内空白区域的总高度
InterHeight: 滚动条出去上下箭头的高度
ThumbHeight: 滑块的高度
 
存在下面比例关系：
si1.nTrackPos/( si1.nMax - si1.nMin + 1 - si1.nPage) = ThumbPos/EmptyHeight;
 
 
 
 
si1.nMax - si1.nMin + 1 - si1.nPage
需要滚动条滚动才能显示的总行数(对应滚动条的空白区域)
si1.nTrackPos
已经滚动的行数(对应滑块的位置)
 
 
则滑块最顶端 ThumbPos = EmptyHeight * si1.nTrackPos/( si1.nMax - si1.nMin + 1 - si1.nPage)



滚动条的长度 = nPage * 滚动区域高度 / ((nMax - nMin)+ nPage); 

http://www.catch22.net/tuts/scrollbars-scrolling



在 SYSMETS2 中，滚动范围设置最小为 0，最大为 NUMLINES-1。当滚动列位置是 0 时，第一行资讯显示在显示区域的顶部；当滚动列的位置是NUMLINES-1时，最後一行显示在显示区域的顶部，并且看不见其他行。可以说 SYSMETS2 滚动范围太大。事实上只需把资讯最後一行显示在显示区域的底部而不是顶部即可。我们可以对 SYSMETS2 作出一些修改以达到此点。当处理 WM_CREATE 讯息时不设置滚动列范围，而是等到接收到 WM_SIZE 讯息後再做此工作：

iVscrollMax = max (0, NUMLINES - cyClient / cyChar) ;
SetScrollRange (hwnd, SB_VERT, 0, iVscrollMax, TRUE) ;

假定NUMLINES等於75，并假定特定视窗大小是：50 （cyChar除以cyClient）。换句话说，我们有 75 行资讯但只有 50 行可以显示在显示区域中。使用上面的两行程式码，把范围设置最小为 0，最大为 25。当滚动列位置等於 0 时，程式显示 0 到 49 行。当滚动列位置等於 1 时，程式显示 1 到 50 行；并且当滚动列位置等於 25（最大值）时，程式显示 25 到 74 行。很明显需要对程式的其他部
分做出修改，但这是可行的。新滚动列函式的一个好的功能是当使用与滚动列范围一样大的页面时，它
已经为您做掉了一大堆杂事。可以像下面的程式码一样使用 SCROLLINFO 结构和

```
SetScrollInfo：
    si.cbSize = sizeof (SCROLLINFO) ;
    si.cbMask = SIF_RANGE | SIF_PAGE ;
    si.nMin = 0 ;
    si.nMax = NUMLINES - 1 ;
    si.nPage = cyClient / cyChar ;
    SetScrollInfo (hwnd, SB_VERT, &si, TRUE) ;
```

这样做之後，Windows 会把最大的滚动列位置限制为 si.nMax - si.nPage +1而不是 si.nMax。像前面那样做出假设：NUMLINES 等於 75 （所以 si.nMax 等於
74），si.nPage 等於 50。这意味著最大的滚动列位置限制为 74 - 50 + 1，即25。这正是我们想要的。
当页面大小与滚动列范围一样大时，会发生什么情况呢？在这个例子中，就是 nPage 等於 75 或更大的情况。Windows 通常隐藏滚动列，因为它并不需要。
如果不想隐藏滚动列，可在呼叫 SetScrollInfo 时使用 SIF_DISABLENOSCROLL，Windows 只是让那个滚动列不能被使用，而不隐藏它。