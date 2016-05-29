//
// A simple C extension module for python, called "hello"
// 
// www.moguf.com  2016-05-28
//

#include <Python.h>
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