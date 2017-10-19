
/* -*- mode: c;  -*-
   file: app_main.c
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <Python.h>
#include "app_main.h"

void app_main(void) {
}

void py_decref(PyObject* o) {
  Py_DECREF(o);
}

const char* pyunicode_as_data(PyObject* o) {
  return PyUnicode_AS_DATA(o);
}

int pyunicode_check(PyObject* o) {
  return PyUnicode_Check(o);
}

/* int pystring_check(PyObject* o) { */
/*   return PyString_Check(o); */
/* } */

int pybool_check(PyObject* o) {
  return  PyBool_Check(o);
}

int pylist_check(PyObject* o) {
  return  PyList_Check(o);
}

int pycallable_check(PyObject* o) {
  return  PyCallable_Check(o);
}

int pylong_check(PyObject* o) {
  return  PyLong_Check(o);
}

int pytuple_check(PyObject* o) {
  return  PyTuple_Check(o);
}
