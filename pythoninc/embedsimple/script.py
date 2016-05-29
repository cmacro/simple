"""
在C中调用Python模块运行。
调用时须要把文件放在程序当前运行目录(保证在搜索目录中)。

www.moguf.com
"""


message = 'hello life...'

def transform(input):
    input = input.replace('life', 'Python')
    return input.upper()
