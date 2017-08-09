
/* -*- mode: c;  -*-
   file: app_main.c
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <Python.h>
#include "app_main.h"

void app_main(void) {
  printf("my text");

}

void py_decref(PyObject* o) {
  Py_DECREF(o);
}

const char* pyunicode_as_data(PyObject* o) {
  return PyUnicode_AS_DATA(o);
}

int pystring_check(PyObject* o) {
  return PyString_Check(o);
}

int pybool_check(PyObject* o) {
  return  PyBool_Check(o);
}

// Convenient wrapper for the Python singleton 'None'
int pynone_check(PyObject* o) {
  return o == Py_None;
}


