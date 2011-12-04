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

/* Memory functions for the native toplevel */

#include "camlnat.h"

/* Management of memory chunks */

struct chunk{
  struct chunk *next;
  uintnat       addr;
  intnat        size;
};

static struct chunk *chunklist = NULL;

static inline unsigned chunk_alignment()
{
#if defined(__amd64__) || defined(__i386__)
  /* Pad sections to 64-byte boundaries to avoid having code
     and data on a single (64-byte) cache line (cf. "Software
     Optimization Guide for the AMD64 Processor"). */
  return 64;
#elif defined(_ARCH_PPC) || defined(_POWER) \
      || defined(__POWERPC__) || defined(__ppc__)
  return 32;
#else
  return sizeof(void *);
#endif
}

static struct chunk *chunk_alloc(intnat size)
{
  struct chunk *chunk;
  static int pagemask = 0;
  void *addr;
  intnat areasize;

  chunk = (struct chunk *)malloc(sizeof(struct chunk));
  if (chunk != NULL) {
    /* Determine the page size on-demand */
    if (pagemask == 0) {
#if defined(_WIN32)
      SYSTEM_INFO systemInfo;
      GetSystemInfo(&systemInfo);
      pagemask = systemInfo.dwPageSize - 1;
#else
      pagemask = getpagesize() - 1;
#endif
    }

    /* Page-align the effective size */
    size = (size + pagemask) & ~pagemask;

    /* Calculate the desired area size */
    if (size <= 64 * 1024)
      areasize = (64 * 1024 + pagemask) & ~pagemask;
    else if (size <= 128 * 1024)
      areasize = size * 2;
    else
      areasize = size;

again:
#if defined(_WIN32)
    addr = VirtualAlloc(NULL, areasize,
                        MEM_COMMIT | MEM_RESERVE,
                        PAGE_EXECUTE_READWRITE);
#else
    addr = mmap(NULL, areasize,
                PROT_EXEC | PROT_READ | PROT_WRITE,
                MAP_ANON | MAP_PRIVATE, -1, 0);
    if (addr == MAP_FAILED)
      addr = NULL;
#endif

    if (addr == NULL) {
      /* Try again with minimal size */
      if (areasize > size) {
        size = areasize;
        goto again;
      }
      free(chunk);
      chunk = NULL;
    }
    else {
      chunk->size = areasize;
      chunk->addr = (uintnat)addr;
    }
  }

  return chunk;
}

value camlnat_mem_alignment(value dummy)
{
  (void)dummy;
  return Val_long(chunk_alignment());
}

value camlnat_mem_reserve(value size)
{
  struct chunk **chunkp;
  struct chunk *chunk;

  assert(Is_long(size));
  assert(Long_val(size) >= 0);
  assert((Long_val(size) % chunk_alignment()) == 0);

  size = Long_val(size);

  for (chunkp = &chunklist; (chunk = *chunkp) != NULL; chunkp = &chunk->next)
    if (chunk->size >= size)
      break;

  if (chunk == NULL) {
    /* Need to allocate a new chunk */
    chunk = chunk_alloc(size);
    if (!chunk) caml_raise_out_of_memory();
    chunk->next = chunklist;
    chunklist = chunk;
  }
  else if (chunkp != &chunklist) {
    /* Move chunk to the head of the list */
    *chunkp = chunk->next;
    chunk->next = chunklist;
    chunklist = chunk;
  }

  assert((chunk->size % chunk_alignment()) == 0);
  assert((chunk->addr % chunk_alignment()) == 0);
  assert(chunk->size >= size);
  assert(chunk == chunklist);
  assert(chunk != NULL);

  return caml_copy_nativeint(chunk->addr);
}

value camlnat_mem_prepare(value addr, value data, value size)
{
  register const char *src;
  register char *dst;
  register mlsize_t len;

  assert(Is_long(size));
  assert(Is_block(addr));
  assert(Is_block(data));
  assert(chunklist != NULL);
  assert(Long_val(size) >= 0);
  assert((Long_val(size) % chunk_alignment()) == 0);
  assert((chunklist->size % chunk_alignment()) == 0);
  assert(Tag_val(addr) == Custom_tag);
  assert(Tag_val(data) == String_tag);
  assert((Nativeint_val(addr) % chunk_alignment()) == 0);
  assert(chunklist->addr <= Nativeint_val(addr));
  assert(chunklist->addr + chunklist->size >= Nativeint_val(addr) + Long_val(size));

  len = Long_val(size);
  if (len != 0) {
    dst = (char *)Nativeint_val(addr);
    src = (const char *)String_val(data);
#if (defined(__clang__) || defined(__GNUC__)) \
    && (defined(__amd64__) || defined(__i386__))
    assert(chunk_alignment() >= 4 * 16);
    if (((uintptr_t)src & 0x0f) == 0) {
      /* src has 16-byte alignment */
      asm volatile(".align  4\n"
                   "1:\t"
                   "sub     $(4*16), %2\n\t"
                   "movdqa  0*16(%1), %%xmm0\n\t"
                   "movdqa  1*16(%1), %%xmm1\n\t"
                   "movdqa  2*16(%1), %%xmm2\n\t"
                   "movdqa  3*16(%1), %%xmm3\n\t"
                   "lea     4*16(%1), %1\n\t"
                   "movntdq %%xmm0, 0*16(%0)\n\t"
                   "movntdq %%xmm1, 1*16(%0)\n\t"
                   "movntdq %%xmm2, 2*16(%0)\n\t"
                   "movntdq %%xmm3, 3*16(%0)\n\t"
                   "lea     4*16(%0), %0\n\t"
                   "jnz     1b\n\t"
                   : "=r"(dst), "=r"(src), "=r"(len)
                   : "0"(dst), "1"(src), "2"(len)
                   : "flags", "memory", "%xmm0", "%xmm1", "%xmm2", "%xmm3");
    }
    else {
      /* src is not 16-byte aligned */
      asm volatile(".align  4\n"
                   "1:\t"
                   "sub     $(4*16), %2\n\t"
                   "movdqu  0*16(%1), %%xmm0\n\t"
                   "movdqu  1*16(%1), %%xmm1\n\t"
                   "movdqu  2*16(%1), %%xmm2\n\t"
                   "movdqu  3*16(%1), %%xmm3\n\t"
                   "lea     4*16(%1), %1\n\t"
                   "movntdq %%xmm0, 0*16(%0)\n\t"
                   "movntdq %%xmm1, 1*16(%0)\n\t"
                   "movntdq %%xmm2, 2*16(%0)\n\t"
                   "movntdq %%xmm3, 3*16(%0)\n\t"
                   "lea     4*16(%0), %0\n\t"
                   "jnz     1b\n\t"
                   : "=r"(dst), "=r"(src), "=r"(len)
                   : "0"(dst), "1"(src), "2"(len)
                   : "flags", "memory", "%xmm0", "%xmm1", "%xmm2", "%xmm3");
    }
#else
    memcpy(dst, src, len);
#endif
  }
  return Val_unit;
}

value camlnat_mem_cacheflush(value addr, value size)
{
  char *beg;
  char *end;

  assert(Is_long(size));
  assert(Is_block(addr));
  assert(Long_val(size) >= 0);
  assert((Long_val(size) % chunk_alignment()) == 0);
  assert((chunklist->size % chunk_alignment()) == 0);
  assert(Tag_val(addr) == Custom_tag);
  assert((Nativeint_val(addr) % chunk_alignment()) == 0);

  beg = (char *)Nativeint_val(addr);
  end = beg + Long_val(size);

#if defined(__APPLE__)
  sys_icache_invalidate(beg, end - beg);
#elif defined(_WIN32)
  FlushInstructionCache(GetCurrentProcess(), beg, end - beg);
#elif (defined(_ARCH_PPC) || defined(_POWER) \
       || defined(__POWERPC__) || defined(__ppc__)) \
      && defined(__GNUC__)
  assert(((uintptr_t)beg % chunk_alignment()) == 0);
  assert(((uintptr_t)end % chunk_alignment()) == 0);
  for (char *ptr = beg; ptr < end; ptr += chunk_alignment())
    asm volatile("dcbf 0, %0" :: "r"(ptr));
  asm volatile("sync");
  for (char *ptr = beg; ptr < end; ptr += chunk_alignment())
    asm volatile("icbi 0, %0" :: "r"(ptr));
  asm volatile("isync");
#elif defined(__arm__) && defined(__GNUC__)
  __clear_cache(beg, end);
#else
  (void)beg;
  (void)end;
#endif
  return Val_unit;
}

value camlnat_mem_commit(value addr, value size)
{
  struct chunk *chunk;

  assert(Is_long(size));
  assert(Is_block(addr));
  assert(chunklist != NULL);
  assert(Long_val(size) >= 0);
  assert((Long_val(size) % chunk_alignment()) == 0);
  assert((chunklist->size % chunk_alignment()) == 0);
  assert(Tag_val(addr) == Custom_tag);
  assert((Nativeint_val(addr) % chunk_alignment()) == 0);
  assert(chunklist->size >= Long_val(size));
  assert(chunklist->addr == Nativeint_val(addr));

  chunk = chunklist;
  chunk->addr += Long_val(size);
  chunk->size -= Long_val(size);
  if (chunk->size < 2 * chunk_alignment()) {
    /* Drop empty chunks from the list */
    chunklist = chunk->next;
    free(chunk);
  }
  return Val_unit;
}
