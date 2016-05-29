"""
import and use a C extension library module
www.moguf.com 2016-05-28
"""

import hello

print(hello.message('C'))
print(hello.message('module ' + hello.__file__))