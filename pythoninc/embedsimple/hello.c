//
// C code runs Python code in this module in embedded mode.
// print hello string
// 
// www.moguf.com  2016-05-28
//

#include <Python.h>

int main()
{
    printf("embed simple\n");
    Py_Initialize();

    PyRun_SimpleString("print('run python ...')");
    PyRun_SimpleString("import script");
    PyRun_SimpleString("print(script.message)");
    PyRun_SimpleString("x = script.message");
    PyRun_SimpleString("print(script.transform(x))");

    Py_Finalize();
}