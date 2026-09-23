/* sys/uio.h shim for Windows */
#ifndef _SYS_UIO_H_
#define _SYS_UIO_H_

#ifdef _WIN32

struct iovec {
  void *iov_base;
  size_t iov_len;
};

#else
#include <sys/uio.h>
#endif

#endif
