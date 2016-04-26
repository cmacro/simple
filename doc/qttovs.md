# 使用VisualStudio2015开发QT项目


一直习惯用VS，做QT项目时，不停的来回切IDE有些不方便。研究了一下QT的编译。

实际QT编译的机制和cmake是相同的，QT的IDE使用pro文件进行项目管理。QMake通过解析pro工程文件，生成makefile进行工程编译。调试QT IDE自身没有调试工具，正常情况下会使用GUN的GDB或VS的CDB等一些外部调试器进行调试的。

实际QT的IDE就是一个外部工具的开发集成环境。可以完全抛开QT的IDE开发QT项目。

> **方法：**  设置的方法是把QT中的命令使用VS中MakeFile工程替换处理。
> **提示：** 这里介绍的不是使用QT插件转换QT工程这种方式

正常情况下完全可以写一个批处理，编译时直接执行一次。不过在**VS**中编译主要一个好处，编译的警告和错误信息可以双击定位。如果不需要看警告和错误信息，也可以直接使用批处理命令进行编译。


<!--more-->


## 1.	安装QT插件

- a)	可以转换QT工程
- b)	在调试时能看到QT类型的变量信息

菜单：<kbd>Tool</kbd>-><kbd>Extensions and Updates…</kbd>
 
![](http://images.moguf.com/imgs/dev/qttovs/01.gif)


## 2.	使用QT插件转换工程

导入QT工程的目的是用于编辑，为了减少配置量。如搜索路径，自能提示和代码跳转，需要正确的源代码目录。

> **问题：**  
> a、	插件工程导入会有些问题，中文不认识会变成？？  
> b、	有绝对路径设置的，可能会出现错误  
> 
> **处理：**  
>    直接使用记事本打开VS的工程，删除即可。  


## 3.	创建编译工程

这步用于取代Qt中的编译处理。

- a、	创建MakeFile工程
 
 ![](http://images.moguf.com/imgs/dev/qttovs/02.gif)
 
- b、	配置编译路径（Intermediate Directory）
- - a)	右击工程->选择工程属性菜单
- - b)	选择 通用->设置 中间路径（构建目录） 和 QT中设置的构建目录一致

设置中间路径目的是为QMake生成的Makefile到指定目录。不设置默认会生成在工程目录（可以正常使用）

![](http://images.moguf.com/imgs/dev/qttovs/03.gif)
 
- c、	设置调试工程
- - a)	Command 直接指向调试exe


![](http://images.moguf.com/imgs/dev/qttovs/04.gif) 

- d、	设置编译**NMake**


实际编译命令即为QT中的编译命令组合，VS中多条命令之间使用 && 进行连接

> **如：**编译 moguf.pro  
>   *. Qmake工程
>   *.联编 
> qmake.exe D:\moguf\code\moguf.pro -r -spec win32-msvc2010 "CONFIG+=debug" && cd /d D:\moguf\make && E:\Qt\Tools\QtCreator\bin\IncrediBuild.bat

其他命令清除、重新编译依次设置。清除命令对应 QT清除步骤设置。重编命令就是 清除命令 + 编译命令

![](http://images.moguf.com/imgs/dev/qttovs/05.gif)

- e、	设置完成



## 4.	编译运行（OK）

- a) 设置MakeFile工程为启动工程
- b) 运行调试

在输出窗口就能看到**Bulid**信息，错误窗口可以看到编译错误和警告（双击可以定位到代码）。
 
![](http://images.moguf.com/imgs/dev/qttovs/06.gif)

