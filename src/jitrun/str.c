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

/* String functions for the native toplevel */

#include "camlnat.h"

static inline unsigned unaligned_get16(const unsigned char *p)
{
#if defined(ARCH_BIG_ENDIAN)
  return (unsigned)p[0] << 8
       | (unsigned)p[1];
#else
  return (unsigned)p[0]
       | (unsigned)p[1] << 8;
#endif
}

static inline uint32 unaligned_get32(const unsigned char *p)
{
#if defined(TARGET_amd64) || defined(TARGET_i386)
  return *(const uint32 *)p;
#elif defined(ARCH_BIG_ENDIAN)
  return (uint32)unaligned_get16(&p[0]) << 16
       | (uint32)unaligned_get16(&p[2]);
#else
  return (uint32)unaligned_get16(&p[0])
       | (uint32)unaligned_get16(&p[2]) << 16;
#endif
}

static inline uint64 unaligned_get64(const unsigned char *p)
{
#if defined(TARGET_amd64) || defined(TARGET_i386)
  return *(const uint64 *)p;
#elif defined(ARCH_BIG_ENDIAN)
  return (uint64)unaligned_get32(&p[0]) << 32
       | (uint64)unaligned_get32(&p[4]);
#else
  return (uint64)unaligned_get32(&p[0])
       | (uint64)unaligned_get32(&p[4]) << 32;
#endif
}

static inline void unaligned_set16(unsigned char *p, unsigned x)
{
#if defined(ARCH_BIG_ENDIAN)
  *p++ = x >> 8;
  *p++ = x;
#else
  *p++ = x;
  *p++ = x >> 8;
#endif
}

static inline void unaligned_set32(unsigned char *p, uint32 x)
{
#if defined(TARGET_amd64) || defined(TARGET_i386)
  *(uint32 *)p = x;
#elif defined(ARCH_BIG_ENDIAN)
  unaligned_set16(&p[0], x >> 16);
  unaligned_set16(&p[2], x);
#else
  unaligned_set16(&p[0], x);
  unaligned_set16(&p[2], x >> 16);
#endif
}

static inline void unaligned_set64(unsigned char *p, uint64 x)
{
#if defined(TARGET_amd64) || defined(TARGET_i386)
  *(uint64 *)p = x;
#elif defined(ARCH_BIG_ENDIAN)
  unaligned_set32(&p[0], x >> 32);
  unaligned_set32(&p[4], x);
#else
  unaligned_set32(&p[0], x);
  unaligned_set32(&p[4], x >> 32);
#endif
}

value camlnat_str_get16(value str, value ofs)
{
  assert(Is_long(ofs));
  assert(Is_block(str));
  assert(Long_val(ofs) >= 0);
  assert(Tag_val(str) == String_tag);
  assert(Long_val(ofs) < caml_string_length(str));

  return Val_long(unaligned_get16(&Byte_u(str, Long_val(ofs))));
}

value camlnat_str_get32(value str, value ofs)
{
  assert(Is_long(ofs));
  assert(Is_block(str));
  assert(Long_val(ofs) >= 0);
  assert(Tag_val(str) == String_tag);
  assert(Long_val(ofs) < caml_string_length(str));

  return caml_copy_int32(unaligned_get32(&Byte_u(str, Long_val(ofs))));
}

value camlnat_str_get64(value str, value ofs)
{
  assert(Is_long(ofs));
  assert(Is_block(str));
  assert(Long_val(ofs) >= 0);
  assert(Tag_val(str) == String_tag);
  assert(Long_val(ofs) < caml_string_length(str));

  return caml_copy_int64(unaligned_get64(&Byte_u(str, Long_val(ofs))));
}

value camlnat_str_set16(value str, value ofs, value val)
{
  assert(Is_long(ofs));
  assert(Is_long(val));
  assert(Is_block(str));
  assert(Long_val(ofs) >= 0);
  assert(Tag_val(str) == String_tag);
  assert(Long_val(ofs) < caml_string_length(str));

  unaligned_set16(&Byte_u(str, Long_val(ofs)), Long_val(val));
  return Val_unit;
}

value camlnat_str_set32(value str, value ofs, value val)
{
  assert(Is_long(ofs));
  assert(Is_block(str));
  assert(Is_block(val));
  assert(Long_val(ofs) >= 0);
  assert(Tag_val(str) == String_tag);
  assert(Tag_val(val) == Custom_tag);
  assert(Long_val(ofs) < caml_string_length(str));

  unaligned_set32(&Byte_u(str, Long_val(ofs)), Int32_val(val));
  return Val_unit;
}

value camlnat_str_set64(value str, value ofs, value val)
{
  assert(Is_long(ofs));
  assert(Is_block(str));
  assert(Is_block(val));
  assert(Long_val(ofs) >= 0);
  assert(Tag_val(str) == String_tag);
  assert(Tag_val(val) == Custom_tag);
  assert(Long_val(ofs) < caml_string_length(str));

  unaligned_set64(&Byte_u(str, Long_val(ofs)), Int64_val(val));
  return Val_unit;
}
