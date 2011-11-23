/***********************************************************************/
/*                                                                     */
/*                              ocamlnat                               */
/*                                                                     */
/*                  Benedikt Meurer, University of Siegen              */
/*                 Marcell Fischbach, University of Siegen             */
/*                                                                     */
/*    Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,    */
/*    Universität Siegen. All rights reserved. This file is distri-    */
/*    buted under the terms of the Q Public License version 1.0.       */
/*                                                                     */
/***********************************************************************/

/* Symbol management for the native toplevel */

#include "camlnat.h"

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

value camlnat_sym_exec(value symbol)
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

value camlnat_sym_load(value symbol)
{
  CAMLparam1 (symbol);
  CAMLlocal1 (sym);

  sym = (value)getsym(NULL, String_val(symbol));
  if (!sym) sym = caml_natdynlink_loadsym(symbol);
  CAMLreturn(sym);
}

value camlnat_sym_add(value symbol, value addr)
{
  addsym(String_val(symbol), (void *)Nativeint_val(addr));
  return Val_unit;
}

value camlnat_sym_get(value symbol)
{
  return caml_copy_nativeint((intnat)camlnat_sym_load(symbol));
}
