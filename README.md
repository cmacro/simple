# 一些示例工程。

在实际工作中使用到的一些简易技巧提炼出来，以供参考。所有代码都在DelphiXE3中调试通过。

## 内容包括

* 标题工具条**TestCaptionToolbar**
* 资源合并工具**MergeIconsRes**
* 建议工具条**AnimateToolbar**


- **TestCaptionToolbar** 通过控制窗体皮肤方式，在标题区域增加一个快速工具条按钮区域。
  未实现：工具条提示Hint显示
          动画效果，鼠标进入不会感觉生硬
          动态添加Action           

- **AnimateToolbar** 一个简易的工具条，加入简单的动画。鼠标在滑入滑出时不会感觉生硬。

- **MergeIconsRes** 图标资源文件合并。对那些零散的PNG图标合并成一个文件，并压缩编入资源文件。
  主要目的：提高加载资源图片的效率，
            降低资源DLL的尺寸

