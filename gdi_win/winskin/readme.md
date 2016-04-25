# 窗体皮肤实现 - 在VC中简单实现绘制（五）

c中简单实现非客户区的绘制。由于都是使用**windowsAPI**，因此思路和delphi版本是一致的。

代码中处理了四个消息，还是比较简单。只有 **WM_WINDOWPOSCHANGING** 消息的处理稍微长些。

> - **WM_NCPAINT**            --- 绘制非客户区
> - **WM_NCCALCSIZE**         --- 重新设置边缘宽度 
> - **WM_NCACTIVATE**         --- 程序切换时重绘非客户去 
> - **WM_WINDOWPOSCHANGING**  --- 重设界面样式 

大致的效果

![最终结果演示](http://www.moguf.com/imgs/dev/winskin/0503.gif)

[具体看blog dev.moguf.com](http://dev.moguf.com/post/devwinskin05)



