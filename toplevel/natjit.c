/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*                  Benedikt Meurer, University of Siegen              */
/*                 Marcell Fischbach, University of Siegen             */
/*                                                                     */
/*    Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,    */
/*    Universität Siegen. All rights reserved. This file is distri-    */
/*    buted under the terms of the Q Public License version 1.0.       */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* JIT support for the native toplevel */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>

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

/* Management of symbols */

struct symbol {
  struct symbol *next;
  void          *addr;
  char           name[1];
};

#define SYMTBLSZ 113

static struct symbol *symtbl[SYMTBLSZ] = { NULL, };

static uint32 symhash(const void *v){
  /* This actually implements the widely used string hash apparently
   * posted by Daniel Bernstein to comp.lang.c once upon a time... */
  const signed char *p = v;
  uint32 h = 5381;
  while (*p != '\0')
    h = (h << 5) + h + *p++;
  return h % SYMTBLSZ;
}

static void addsym(char *name, void *addr){
  mlsize_t namelen = strlen(name);
  struct symbol *sym = malloc(sizeof(struct symbol) + namelen);
  struct symbol **symtblp = &symtbl[symhash(name)];
  memcpy(sym->name, name, namelen + 1);
  sym->addr = addr;
  sym->next = *symtblp;
  *symtblp = sym;
}

static void *getsym(char *module, char *name){
  void *addr = NULL;
  struct symbol *sym;
  char *fullname = name;
  if (module != NULL) {
    fullname = malloc(strlen(module) + strlen(name) + 5);
    sprintf(fullname, "caml%s%s", module, name);
  }
  for (sym = symtbl[symhash(fullname)]; sym != NULL; sym = sym->next){
    if (strcmp(sym->name, fullname) == 0){
      addr = sym->addr;
      break;
    }
  }
  if (name != fullname) free(fullname);
  return addr;
}

/* OS support */

#if defined(OS_Unix)

#include <sys/types.h>
#include <sys/mman.h>
#ifdef HAS_UNISTD
# include <unistd.h>
#endif

#define PROT_RW  (PROT_READ | PROT_WRITE)
#define PROT_RWX (PROT_EXEC | PROT_RW)

static void *mmap_p(mlsize_t size, int prot){
  return mmap(NULL, size, prot, MAP_ANON | MAP_PRIVATE, -1, 0);
}

#elif defined(OS_Win32)

#include <windows.h>

#ifdef _MSC_VER
typedef signed __int16   int16_t;
typedef unsigned __int16 uint16_t;
#endif

#define PROT_RW  PAGE_READWRITE
#define PROT_RWX PAGE_EXECUTE_READWRITE

#define MAP_FAILED NULL

static int getpagesize(void){
  SYSTEM_INFO systemInfo;
  GetSystemInfo(&systemInfo);
  return systemInfo.dwPageSize;
}

static void *mmap_p(mlsize_t size, int prot){
  return VirtualAlloc(NULL, size, MEM_COMMIT | MEM_RESERVE, prot);
}

static void munmap(void *addr, mlsize_t size){
  VirtualFree(addr, size, MEM_RELEASE);
}

static void mprotect(void *addr, mlsize_t size, int prot){
  DWORD oldProt = 0;
  VirtualProtect(addr, size, prot, &oldProt);
}

#endif

value camlnat_jit_getint16(value str, value ofs)
{
  return Val_long(*(const int16_t *)(String_val(str) + Long_val(ofs)));
}

value camlnat_jit_getint32(value str, value ofs)
{
  return caml_copy_int32(*(const int32 *)(String_val(str) + Long_val(ofs)));
}

value camlnat_jit_getint64(value str, value ofs)
{
  return caml_copy_int64(*(const int64 *)(String_val(str) + Long_val(ofs)));
}

value camlnat_jit_putint16(value str, value ofs, value val)
{
  *((int16_t *)(String_val(str) + Long_val(ofs))) = Long_val(val);
  return Val_unit;
}

value camlnat_jit_putint32(value str, value ofs, value val)
{
  *((int32 *)(String_val(str) + Long_val(ofs))) = Int32_val(val);
  return Val_unit;
}

value camlnat_jit_putint64(value str, value ofs, value val)
{
  *((int64 *)(String_val(str) + Long_val(ofs))) = Int64_val(val);
  return Val_unit;
}

value camlnat_jit_malloc(value text_size, value data_size)
{
#define ABS(x) (((x) < 0) ? -(x) : (x))
#define ALIGN(x, n) ((((x) + ((n) - 1)) / (n)) * (n))
  CAMLparam2 (text_size, data_size);
  CAMLlocal1 (res);
  static char *data_ptr = NULL, *data_end = NULL;
  static char *text_ptr = NULL, *text_end = NULL;
  mlsize_t tsize = ALIGN(Long_val(text_size), 16);
  mlsize_t dsize = ALIGN(Long_val(data_size), 16);
  mlsize_t psize, size;
  char *area, *text, *data;

  /* Memory allocation tries to reuse already allocated memory,
   * which works in many cases. For the amd64 case we ensure
   * that all data memory is within 32bit range from the text
   * memory allocated.
   */
  for (;;){
    /* Check if leftover space is sufficient */
    if (dsize < data_end - data_ptr && tsize < text_end - text_ptr){
      text = text_ptr; text_ptr += tsize;
      data = data_ptr; data_ptr += dsize;
      break;
    }

    psize = getpagesize();
    if (dsize < data_end - data_ptr){
      /* Need new text area */
      size = 2 * ALIGN(tsize, psize);
      area = (char *)mmap_p(size, PROT_RWX);
      if (area == (char *)MAP_FAILED) caml_raise_out_of_memory();
      text_ptr = area;
      text_end = area + size;
    }
    else if (tsize < text_end - text_ptr){
      /* Need new data area */
      size = 2 * ALIGN(dsize, psize);
      area = (char *)mmap_p(size, PROT_RWX);
      if (area == (char *)MAP_FAILED) caml_raise_out_of_memory();
      mprotect(area, size, PROT_RW);
      data_ptr = area;
      data_end = area + size;
    }
    else{
      /* Need both new data and new text area */
      mlsize_t tsize_aligned = 2 * ALIGN(tsize, psize);
      mlsize_t dsize_aligned = 2 * ALIGN(dsize, psize);
      size = tsize_aligned + dsize_aligned;
      area = (char *)mmap_p(size, PROT_RWX);
      if (area == (char *)MAP_FAILED) caml_raise_out_of_memory();
      mprotect(area + tsize_aligned, dsize_aligned, PROT_RW);
      text_ptr = area;
      text_end = text_ptr + tsize_aligned;
      data_ptr = text_end;
      data_end = data_ptr + dsize_aligned;
    }

#ifdef TARGET_amd64
    if (ABS(text_end - data_ptr) >= 2147483647
        || ABS(data_end - text_ptr) >= 2147483647){
      /* Out of 32bit addressing range, try again... */
      munmap(area, size);
      text_ptr = text_end = NULL;
      data_ptr = data_end = NULL;
    }
#endif
  }

  res = caml_alloc_tuple(2);
  Field(res, 0) = (value)caml_copy_nativeint((intnat)text);
  Field(res, 1) = (value)caml_copy_nativeint((intnat)data);
  CAMLreturn(res);
#undef ALIGN
#undef ABS
}

value camlnat_jit_memcpy(value dst, value src, value size)
{
  memcpy((void *)Nativeint_val(dst), String_val(src), Long_val(size));
  return Val_unit;
}

value camlnat_jit_loadsym(value symbol)
{
  CAMLparam1 (symbol);
  CAMLlocal1 (sym);
  
  sym = (value)getsym(NULL, String_val(symbol));
  if (!sym) sym = caml_natdynlink_loadsym(symbol);
  CAMLreturn(sym);
}

value camlnat_jit_addsym(value symbol, value addr)
{
  addsym(String_val(symbol), (void *)Nativeint_val(addr));
  return Val_unit;
}

value camlnat_jit_getsym(value symbol)
{
  return caml_copy_nativeint((intnat)camlnat_jit_loadsym(symbol));
}

value camlnat_jit_run(value symbol)
{
  CAMLparam1 (symbol);
  CAMLlocal1 (result);
  void *sym;
  void *sym2;

#define optsym(n) getsym(unit,n)
  char *unit;
  void (*entrypoint)(void);

  unit = String_val(symbol);

  sym = optsym("__frametable");
  if (sym) caml_register_frametable(sym);

  sym = optsym("");
  if (sym) caml_register_dyn_global(sym);

  sym = optsym("__data_begin");
  sym2 = optsym("__data_end");
  if (sym && sym2) caml_page_table_add(In_static_data, sym, sym2);

  sym = optsym("__code_begin");
  sym2 = optsym("__code_end");
  if (sym && sym2) caml_page_table_add(In_code_area, sym, sym2);

  entrypoint = optsym("__entry");
  if (entrypoint) result = caml_callback((value)&entrypoint, 0);
  else result = Val_unit;

#undef optsym

  CAMLreturn (result);
}
