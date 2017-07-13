
/* -*- mode: c;  -*-
   file: app_main.h
*/

#ifndef __APP_MAIN_H__
#define __APP_MAIN_H__

void app_main(void);
void py_decref(PyObject *o);
const char* pyunicode_as_data(PyObject *o);

#endif /* APP_MAIN_H */
