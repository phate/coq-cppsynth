#ifndef JSYN_UTIL_ASSERT_HPP
#define JSYN_UTIL_ASSERT_HPP

#include <assert.h>

#ifdef JSYN_ENABLE_ASSERTS
# define JSYN_ASSERT(x) assert(x)
#else
# define JSYN_ASSERT(x) (void)(x)
#endif

#endif
