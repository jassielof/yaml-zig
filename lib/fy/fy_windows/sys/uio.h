#ifndef _SYS_UIO_H_
#define _SYS_UIO_H_
#endif

#ifndef _FY_IOVEC_DEFINED
#define _FY_IOVEC_DEFINED

#include <stddef.h>

struct iovec {
  void *iov_base;
  size_t iov_len;
};
#endif
