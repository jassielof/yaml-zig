#ifndef CONFIG_H
#define CONFIG_H

/* Windows compatibility for sys/uio.h */
#ifdef _WIN32
#ifndef _SYS_UIO_H_
#define _SYS_UIO_H_
#include <stddef.h>
struct iovec {
  void *iov_base;
  size_t iov_len;
};
#endif
#endif

#if defined(__has_include)
#if __has_include(<alloca.h>)
#define HAVE_ALLOCA_H 1
#endif
#if __has_include(<byteswap.h>)
#define HAVE_BYTESWAP_H 1
#endif
#endif

#if defined(__GNUC__) || defined(__clang__)
#define HAVE___BUILTIN_BSWAP16 1
#define HAVE___BUILTIN_BSWAP32 1
#define HAVE___BUILTIN_BSWAP64 1
#endif

#if defined(__unix__) || defined(__APPLE__) || defined(_WIN32)
#define HAVE_DECL_ENVIRON 1
#endif

#if defined(__linux__)
#define HAVE_QSORT_R 1
#define HAVE_MREMAP 1
#else
#define HAVE_QSORT_R 0
#define HAVE_MREMAP 0
#endif

#define TARGET_HAS_SSE2 0
#define TARGET_HAS_SSE41 0
#define TARGET_HAS_AVX2 0
#define TARGET_HAS_AVX512 0
#define TARGET_HAS_NEON 0

#define HAVE_LIBCLANG 0
#define HAVE_CLANG_BLOCKS 0
#define HAVE_HEAP_TRAMPOLINES 0

#define VERSION "vendored"

#endif
