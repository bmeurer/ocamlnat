/***********************************************************************/
/*                                                                     */
/*                              ocamlnat                               */
/*                                                                     */
/*                  Benedikt Meurer, University of Siegen              */
/*                                                                     */
/*    Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,    */
/*    Universität Siegen. All rights reserved. This file is distri-    */
/*    buted under the terms of the Q Public License version 1.0.       */
/*                                                                     */
/***********************************************************************/

#ifndef CAMLNAT_H
#define CAMLNAT_H

#include <assert.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#ifdef __APPLE__
#include <libkern/OSCacheControl.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAS_MMAP
# include <sys/types.h>
# include <sys/mman.h>
#endif
#ifdef HAS_UNISTD
# include <unistd.h>
#endif
#ifdef _WIN32
# include <windows.h>
#endif

/* Externals */

#define Not_in_heap 0
#define In_heap 1
#define In_young 2
#define In_static_data 4
#define In_code_area 8

extern value caml_natdynlink_loadsym(value);
extern void caml_register_frametable(intnat *);
extern void caml_register_dyn_global(void *);
extern int caml_page_table_add(int, void *, void *);

#endif /* !CAMLNAT_H */
