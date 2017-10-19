
/* -*- mode: c;  -*-
   file: app_main.h
*/

#ifndef __APP_MAIN_H__
#define __APP_MAIN_H__

void app_main(void);
void py_decref(PyObject *o);
const char* pyunicode_as_data(PyObject *o);
int pystring_check(PyObject* o);
int pybool_check(PyObject* o);
int pylist_check(PyObject* o);
int pycallable_check(PyObject* o);
int pylong_check(PyObject* o);
int pytuple_check(PyObject* o);

#endif /* APP_MAIN_H */
