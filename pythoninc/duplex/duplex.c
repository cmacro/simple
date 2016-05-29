//
// c API module, test c embedding and extending
// 
// www.moguf.com 2016-05-29
//

#include <Python.h>
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
