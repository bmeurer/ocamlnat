/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*                  Benedikt Meurer, University of Siegen              */
/*                                                                     */
/*    Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,    */
/*    Universität Siegen. All rights reserved. This file is distri-    */
/*    buted under the terms of the Q Public License version 1.0.       */
/*                                                                     */
/***********************************************************************/

/* String functions for the native toplevel */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>

value camlnat_str_get16(value str, value ofs)
{
  unsigned char *p = &Byte_u(str, Long_val(ofs));
#if defined(ARCH_BIG_ENDIAN)
  unsigned x = (unsigned)p[0] << 8
             | (unsigned)p[1];
#else
  unsigned x = (unsigned)p[0]
             | (unsigned)p[1] << 8;
#endif
  return Val_long(x);
}

value camlnat_str_get32(value str, value ofs)
{
  unsigned char *p = &Byte_u(str, Long_val(ofs));
#if defined(TARGET_amd64) || defined(TARGET_i386)
  uint32 x = *(const uint32 *)p;
#elif defined(ARCH_BIG_ENDIAN)
  uint32 x = (uint32)p[0] << 24
           | (uint32)p[1] << 16
           | (uint32)p[2] << 8
           | (uint32)p[3];
#else
  uint32 x = (uint32)p[0]
           | (uint32)p[1] << 8
           | (uint32)p[2] << 16
           | (uint32)p[3] << 24;
#endif
  return caml_copy_int32(x);
}

value camlnat_str_get64(value str, value ofs)
{
  unsigned char *p = &Byte_u(str, Long_val(ofs));
#if defined(TARGET_amd64) || defined(TARGET_i386)
  uint64 x = *(const uint64 *)p;
#elif defined(ARCH_BIG_ENDIAN)
  uint64 x = (uint64)p[0] << 56
           | (uint64)p[1] << 48
           | (uint64)p[2] << 40
           | (uint64)p[3] << 32
           | (uint64)p[4] << 24
           | (uint64)p[5] << 16
           | (uint64)p[6] << 8
           | (uint64)p[7];
#else
  uint64 x = (uint64)p[0]
           | (uint64)p[1] << 8
           | (uint64)p[2] << 16
           | (uint64)p[3] << 24
           | (uint64)p[4] << 32
           | (uint64)p[5] << 40
           | (uint64)p[6] << 48
           | (uint64)p[7] << 56;
#endif
  return caml_copy_int64(x);
}

value camlnat_str_set16(value str, value ofs, value val)
{
  unsigned char *p = &Byte_u(str, Long_val(ofs));
  unsigned x = Long_val(val);
#if defined(ARCH_BIG_ENDIAN)
  *p++ = x >> 8;
  *p++ = x;
#else
  *p++ = x;
  *p++ = x >> 8;
#endif
  return Val_unit;
}

value camlnat_str_set32(value str, value ofs, value val)
{
  unsigned char *p = &Byte_u(str, Long_val(ofs));
  uint32 x = Int32_val(val);
#if defined(TARGET_amd64) || defined(TARGET_i386)
  *(uint32 *)p = x;
#elif defined(ARCH_BIG_ENDIAN)
  *p++ = x >> 24;
  *p++ = x >> 16;
  *p++ = x >> 8;
  *p++ = x;
#else
  *p++ = x;
  *p++ = x >> 8;
  *p++ = x >> 16;
  *p++ = x >> 24;
#endif
  return Val_unit;
}

value camlnat_str_set64(value str, value ofs, value val)
{
  unsigned char *p = &Byte_u(str, Long_val(ofs));
  uint64 x = Int64_val(val);
#if defined(TARGET_amd64) || defined(TARGET_i386)
  *(uint64 *)p = x;
#elif defined(ARCH_BIG_ENDIAN)
  *p++ = x >> 56;
  *p++ = x >> 48;
  *p++ = x >> 40;
  *p++ = x >> 32;
  *p++ = x >> 24;
  *p++ = x >> 16;
  *p++ = x >> 8;
  *p++ = x;
#else
  *p++ = x;
  *p++ = x >> 8;
  *p++ = x >> 16;
  *p++ = x >> 24;
  *p++ = x >> 32;
  *p++ = x >> 40;
  *p++ = x >> 48;
  *p++ = x >> 56;
#endif
  return Val_unit;
}
