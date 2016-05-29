
> **概述：** 为什么要集成脚本，怎么在工程中集成Python脚本。

在做比较大型的工程时，一般都会分核心层和业务层。核心层要求实现高效和稳定的基础功能，并提供调用接口供业务层调用的一种标准的框架划分。在实际中根据需求会拆分的更细。

外部的表现形式就是一个核心动态库，带着一堆业务业务动态库。通过一个调度程序把这些链接起来，外加一堆配置文件，就形成一个完成的项目。

这种模式在一个团队开发中，工作职责比较容易划分。制定API接口后，开发工作基本可以并行实现，包括后期的功能测试（白盒、黑盒）。不管工程使用什么语言，基本都是如此。


[c语言][devs]无疑是很强大而又灵活的，但是开发比较复杂，开发工期比较长。全部使用[c/c++][devs] 进行开发的话，编译调试整合发布，就需要大量时间，包括运营维护的话，呵呵～ 。整个产品的生命周期需要投入大量的人力来维持这个产品。特别是对做定制的最终用户的话，那就可能就是一场旷日持久战。

<!--more-->

刚才说的一般工程都会包括2方面，核心和扩展。是公司级产品的话，一定会出于盈利问题，会对核心做保密（商业机密）。扩展是在核心基础上实现的，很大程度上保密等级就没那么高，甚至可以是开放式的。方便有一定能力的用户直接扩展。这是最理想的一种情况。


如果业务层还是使用[c/c++][devs]的话，估计用户没有几个有能力或者说不太愿意去做扩展。只能公司团队进行维护和扩展。当然开源项目除外，活跃的开源项目还是有很多大侠们愿意去扩展的。当降低扩展的难度，不仅可以缩短开发周期降低成本，降低运营维护成本。如果产品够优秀，还能吸引有一定能力的客户来帮忙做扩展。扩展简单方便，用户自己都能扩展，项目运营成本必定降低。这就皆大欢喜。


脚本是一个非常方便的东西，不需要编译直接运行就能看到结果。不用考虑大量系统相关的开发技巧，更贴近实际业务的描述，修正问题不需要重新编译发布，维护非常方便。能大大提供生产效率，这就是我们所须要的。现在流行的脚本语言有很多Ruby、Perl、[Python][python]、Lua、Javascript等等。。。，反正很多。


> **脚本语言比较:**  
>
> 1、[**python：**][python] 简单易学，有大量扩展可以使用。  
> 2、**Ruby：** 魔幻型纯面向对象语言，非常灵活，学习相对其他语言有一定难度。  
> 3、**Lua：** 超轻量级，性能高效。很多游戏使用Lua提供扩展。  

使用那种脚本作为扩展，要看实际项目和现有资源的情况而定。本文只介绍[python][python]的集成。


废话这么多，主要的目的就下面几个。

> **扩展目的：**  
>   
> **1、高可配性。** 解决一些简单配置不能实现的组织、回调功能，避免改动重编重发布。  
> **2、为了使用已有的库。** 如原来你有很多积累，写了一些适合工程开发的库。  
> **3、优化程序提升性能。** 对脚本程序使用库的一种情况。  


**集成：** 就是通过简单、直接和快速的在不同语言直接调度切换控制，属于无缝连接。就像使用同一种语言在不同的动态库之间调度。而不是那种使用套接字和管道等的间接调度。

混合开发中，不管[python][python]或C都可以作为“上层”。因此两方面都要提供入口提供对方调度。这个其实和正常的同一语言编写的插件模式是一致的。核心提供接口和注册入口，扩展注册入口并调用接口。

> **嵌入接口：** C程序中运行Python的代码  
> **扩展接口：** Python程序中运行C的代码库  


![](http://imgs.moguf.com/imgs/dev/pythoninc/0101.gif)


> **测试环境：**  
> window 7  
> vs2015  
> python 3.5  



## 1、准备环境

使用的Python是3.5版本的。

首先肯定是须要个python的运行环境，可以直接从官网[python.org](https://www.python.org)下载Python3.x 进行安装。建议直接下源代码编译，因为里面有很多代码可以参考。


### windows配置

python安装路径以下为例`d:\python`，在系统的用户环境变量中添加。


**1、** 增加python搜索路径，方便代码运行调试

```
path=d:\python;d:\python\pcbuild\win32
```

**2、** 增加python环境路径。加载模块时默认会从配置路径中搜索。

```
PYTHONPATH=.;d:\python\lib;d:\python\pcbuild\win32;D:\Python\Lib\site-packages;d:\python
```

**3、** 增加编译路径。

```
PyInc=d:\python\include;d:\python\pc
PyLib=d:\python\pcbuild\win32
```

方便VS搜索路径配置。在工程中，只需引用配置变量路径，可以直接使用$(PyInc) 和 $(PyLib)。


> **提示：**  由于是自己编译的，一些环境参数没有须要自己加。注意大小写

## 2.1、嵌入Python第一个简单工程

### 创建测试工程

在VS中创建一个空的VC++控制台程序。在工程选择的搜索目录中加入`$(PyInc)`。

选择菜单：<kbd>Project</kbd>-><kbd>Properties...</kbd>

在`VC++ Directories`分类的`Include Directories`中加入先前定义的环境变量`$(PyInc)`。确保Python的头文件能搜索到。

![](http://imgs.moguf.com/imgs/dev/pythoninc/0104.gif)

使用环境变量的一个好处是，方便不同机器不同环境的切换，不需要修改工程配置。

> **提示：** 可以不建工程，直接使用makefile方式进行编译。

### 创建script.py脚本

在工程中创建一个新文件，直接改名为`script.py`，复制下面内容。

> **注意：** 编码设置为UTF-8

```
"""
在C中调用Python模块运行。
调用时须要把文件放在程序当前运行目录(保证在搜索目录中)。
www.moguf.com  2016-05-28
"""

message = 'hello life...'

def transform(input):
    input = input.replace('life', 'Python')
    return input.upper()
```

在Python环境中运行这个脚本，可以得到下面结果。可以看到打印出 `hello life...` 和转换后的字符串 `HELLO PYTHON...`


```
Microsoft Windows [版本 6.1.7601]
版权所有 (c) 2009 Microsoft Corporation。保留所有权利。

C:\Users\CrystalIce>cd /d D:\Dev\MySimple\pythoninc\embedsimple

D:\Dev\MySimple\pythoninc\embedsimple>python
Running Release|Win32 interpreter...
Python 3.5.0 (default, Nov  4 2015, 21:58:28) [MSC v.1900 32 bit (Intel)] on win32
Type "help", "copyright", "credits" or "license" for more information.
>>> import script
>>> print(script.message)
hello life...
>>> x = script.message
>>> print(script.transform(x))
HELLO PYTHON...
>>> 
```

### 创建hello.c 

在工程中添加一个新文件，命名为`hello.c`。复制下面内容。

```
//
// C code runs Python code in this module in embedded mode.
// print hello string
// 
// www.moguf.com  2016-05-28
//

#include <python.h>

int main()
{
    Py_Initialize();

    PyRun_SimpleString("print('run python ...')");
    PyRun_SimpleString("import script");
    PyRun_SimpleString("print(script.message)");
    PyRun_SimpleString("x = script.message");
    PyRun_SimpleString("print(script.transform(x))");

    Py_Finalize();
}
```


### 运行程序 

在程序最后下个断点，方便查看运行结果。直接运行程序可以看到下面结果。

```
run python ...
hello life...
HELLO PYTHON...

```

### 基本调用流程解析

使用C程序运行Python脚本代码，可以通过使用Python字符串，调用Python对象或模板之类的所有操作。

> **流程：**  
> 1、初始化Python解析器  
> 2、执行Python代码，字符串，对象或模块。  
> 3、关闭Python解析器。  

上面代码嵌入过程很容易。但在实际使用中想要更好的整合，须要了解提供的API和不同语言之间的转换。


### Python嵌入C的基础API

下面几个基础API，在C中能很容易的执行Python脚本中的代码。包括字典、数组和对象。当然想要更好的混合交互须要熟悉所有的API。

C API 调用 | Python 对应
--- | ---
PyImport_ImportModel | import module
PyImport_ReloadModule | reload(module)
PyImport_GetModuleDict | `module.__dict__`
PyDict_GetItemString | dict[key]
PyDict_SetItemString | dict[key] = value
PyDict_New | dict = {}
PyObject_GetAttrString | getattr(obj, attr)
PyObject_SetAttrString | setattr(obj, attr, val)
PyObject_CallObject | funcobj(*argstuple)
PyEval_CallObject | funcobj(*argstuple)
PyRun_String | eval(exprstr) ,  exec(stmtstr)
PyRun_File | exec(open(filename().read())


> **建议：** 去官网下载一个手册，方便查看API。https://docs.python.org/3/download.html



## 2.2、使用C扩展Python

**2.1**的内容只是通过C程序调用Python脚本，要让Python脚本能调用C代码，就须要扩展。用C扩展Python功能那就简单很多。有很多实例可以参考。Python源代码就是宝库。

### 创建扩展工程 hello

在VS中创建一个空的动态库`hello`工程（**先不要改名**）。在工程配置中增加搜索路径。

在VS生成时有些特殊。生成的后缀选择**.pyd**主要是为防止和系统**.dll**产生冲突。

在工程选项界面中设置工程输出名称为`$(ProjectName)_d`，输出扩展名称为`.pyd`。

![](http://imgs.moguf.com/imgs/dev/pythoninc/0102.gif)

并在**Linker**页面**Input**组中设置库依赖为`python35_d.lib`

![](http://imgs.moguf.com/imgs/dev/pythoninc/0103.gif)


> **不同的编译模式的设置：**  
> **Release** 下使用的依赖库为`pythonXY.lib`  
> **Debug** 下使用依赖库为 `pythonXY_d.lib` 
>
> **注意：**   
> **debug** 模式生成应为 **hello_d.pyd**  
> **release** 模式生成应为 **hello.pyd**  


### 创建hello.c扩展代码

在工程中新建hello.c文件，复制下面内容。

```
//
// A simple C extension module for python, called "hello"
// 
// www.moguf.com  2016-05-28
//

#include <python.h>
#include <string.h>

//
// module functions
//
static PyObject *                                 // returns object
message(PyObject *self, PyObject *args)           
{                                                 
    char *fromPython, result[1024];
    if (!PyArg_Parse(args, "(s)", &fromPython))  // convert Python -> C 
        return NULL;                             //  exception null = raise 
    else {
        strcpy(result, "Hello , ");                // build up C string
        strcat(result, fromPython);               // add passed Python string 
        return Py_BuildValue("s", result);        // convert C -> Python 
    }
}

//
// registration methods table 
static PyMethodDef hello_methods[] = {
    { "message",  message, METH_VARARGS, "func doc" },    // format: name, &func, fmt, doc 

    { NULL, NULL, 0, NULL }                               // end
};

// module definition structure
static struct PyModuleDef hellomodule = {
    PyModuleDef_HEAD_INIT,
    "hello",         // module name
    "mod doc",       // module documentation,
    -1,              
    hello_methods    // methods table
};

//
// module initializer
PyMODINIT_FUNC
PyInit_hello()                         
{                                      
    return PyModule_Create(&hellomodule);
}
```

上面的代码主要分为四块。

> **第一块：**  模块功能实现函数  
> **第二块：** 注册功能函数  
> **第三块：** 定义模块申明  
> **第四块：** 初始化模块。动态加载就不需要这块内容，集成时会使用动态加载。  

通过上述定义为Python脚本调用提供访问入口，这就是通常所说的胶水代码。具体定义直接看代码注释，就不啰嗦了。

这里须要注意的是定义中的名称**hello**。在第三块模型注册的时候是名称为**hello**、第四块中函数的初始化名称`PyInit_hello()`。在python3中的名称规定比较严格，初始化函数名称格式为`PyInit_xxx`, xxx为注册的模块名称。

即对**Python扩展工程**中的`工程名称`，`注册名称`和`初始化名称`须要保持一致。

### 编译测试运行

编译hello工程（debug版本），在Python调试版本下运行。python调试环境使用`python_d`命令进入。

可以看到下面结果，就说明OK了

```
Microsoft Windows [版本 6.1.7601]
版权所有 (c) 2009 Microsoft Corporation。保留所有权利。

C:\Users\CrystalIce>cd /d D:\Dev\MySimple\pythoninc\Debug

D:\Dev\MySimple\pythoninc\Debug>dir
 驱动器 D 中的卷是 Docs
 卷的序列号是 0002-2203

 D:\Dev\MySimple\pythoninc\Debug 的目录

2016-05-28  23:10    <dir>          .
2016-05-28  23:10    <dir>          ..
2016-05-28  23:10               639 hello_d.exp
2016-05-28  23:10           247,520 hello_d.ilk
2016-05-28  23:10             1,718 hello_d.lib
2016-05-28  23:10           503,808 hello_d.pdb
2016-05-28  23:10            35,840 hello_d.pyd
               5 个文件        789,525 字节
               2 个目录 27,394,551,808 可用字节

D:\Dev\MySimple\pythoninc\Debug>python_d
Python 3.5.0 (default, Nov  4 2015, 21:57:44) [MSC v.1900 32 bit (Intel)] on win32
Type "help", "copyright", "credits" or "license" for more information.
>>> import hello
>>> print(hello.message('C'))
Hello , C
>>> print(hello.message('module ' + hello.__file__))
Hello , module D:\Dev\MySimple\pythoninc\Debug\hello_d.pyd
>>>
```

如果在运行调试中出现下面情况，是Python找不到**hello**模块导致的。
```
>>> import hello
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
ImportError: No module named 'hello'
```

**大致原因：**

编译的模块名称有问题，加载不到模块。Python在debug环境下会调用 xxx_d.pyd，release下会调用 xxx.pyd。


### 创建hellouse.py 调用hello扩展模块

在工程中新建一个hellouse.py，用于调度hello扩展模块。

> **注意：** 编码设置为UTF-8

内容如下：

```
"""
import and use a C extension library module
www.moguf.com 2016-05-28
"""

import hello

print(hello.message('C'))
print(hello.message('module ' + hello.__file__))
```

把这个脚本复制到`hello_d.pyd`扩展库所在目录，并执行 。可以看到和刚才测试输出的结果是一致的。

```
D:\Dev\MySimple\pythoninc\Debug>python_d hellouse.py
Hello , C
Hello , module D:\Dev\MySimple\pythoninc\Debug\hello_d.pyd
```

> **相关编译问题：**   
> 如果自己建的工程编译或调试，老出现状况。可以直接使用Python提供的`PC\example_nt`VS示例工程作为参考。


## 2.3、集成Python，实现双工

先前的的两个示例都是单方面调用，c调用[Python][python] 和 Python调用c的扩展模型。并没有交互。在实际工程中不太可能有这种情况，一定是相互交叉调用。

### 创建duplex工程

在VS中创建一个空的控制台程序，并设置Python代码搜索路径，参照2.1。

### 创建duplex.c 文件 

这个原文件包括了脚本调用和胶水代码的实现。和2.1、2.2的内容基本一致。其中主要的差异在Python模块的注册上

```
PyImport_AppendInittab("hello_api", &PyInit_hello_api);
```

实际对外注册的模块在程序启动时执行，并没有作导出。


文件内容如下。

```
//
// c API module, test c embedding and extending
// 
// www.moguf.com 2016-05-29
//

#include <python.h>
#include <string.h>

void helloWorld(char *param)
{
    if (param)
        printf("It's c, hello %s", param);
    else 
        printf("It's c, hello ");
}

static PyObject *
message(PyObject *self, PyObject *args)
{
    char *fromPython;
    if (!PyArg_Parse(args, "(s)", &fromPython))
        helloWorld(NULL);
    else
        helloWorld(fromPython);

    return Py_BuildValue("");
}

static PyMethodDef hello_methods[] = {
    { "message",  message, METH_VARARGS, "func doc" },

    { NULL, NULL, 0, NULL }                               // end
};

static struct PyModuleDef hello_api = {
    PyModuleDef_HEAD_INIT, 
    "hello_api",         
    "mod doc", 
    -1,
    hello_methods
};

static PyObject*
PyInit_hello_api(void)
{
    return PyModule_Create(&hello_api);
}

int main(int argc, char** argv)
{
    PyObject* module;
    PyObject* func;

    // add c api to modules
    PyImport_AppendInittab("hello_api", &PyInit_hello_api);

    Py_Initialize();
    if (!Py_IsInitialized()) {
        PyErr_Print();
        printf("Couldn't init python");
        return -1;
    }

    module = PyImport_ImportModule("plugins");
    if (module) {
        func = PyObject_GetAttrString(module, "helloWorld");
        if (func && PyCallable_Check(func)) {
            PyObject* pArgs = NULL;
            PyObject* pReturnVal = PyObject_CallObject(func, pArgs);
        }
        else {
            PyErr_Print();
            printf("error: no func\n");
        }

        Py_XDECREF(func);
        Py_DECREF(module);
    }
    else {
        PyErr_Print();
        printf("err: no module");
    }

    Py_Finalize();
    return 0;
}

```

### 创建脚本plugins.py

内容如下

```
"""
Module to test c embedding and extending
www.moguf.com 2016-05-29
"""

import hello_api

def helloWorld():
    print("it's Python, Hello C")
    hello_api.message('python')
    return
```

### 运行测试

在 main函数结束的位置设置断点，这用方便查看结果。运行程序。

```
it's Python, Hello C
It's c, hello python
```

可以看到，第一行打印是由Python脚本实现输出，第二行是由python调用程序的API实现打印输出。




## 3、后续

通过上述简单的三个实例实现了c语言和Python脚本的集成。简单、直接和快速的在不同语言直接调度切换控制。

由于Python开始时本身就是基于C写的，所有对c的支持是非常好的。能在c/c++中很方便的进行集成。不过要想更好的实现脚本和C进行交互，那就须要熟悉并使用提供的API。

后续将会使用Python脚本作为插件扩展一种模式，在实际工程中实现业务的一些方案。


> **相关参考：**  
> 1、官方帮助 https://docs.python.org/3/
> 2、源代码：https://github.com/cmacro/simple/tree/master/pythoninc



[devs]: http://www.moguf.com/category/devs
[python]: http://www.moguf.com/category/python</string.h></python.h></module></stdin></dir></dir></string.h></python.h></python.h>