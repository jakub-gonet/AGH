#include "common.h"
#include <math.h>

#define _POSIX_C_SOURCE 200809L

void p_get_current_time(time_t* s, long* ms) {
  struct timespec spec;
  clock_gettime(CLOCK_REALTIME, &spec);
  *s = spec.tv_sec;
  *ms = round(spec.tv_nsec / 1.0e6);
  if (*ms > 999) {
    (*s)++;
    (*ms) = 0;
  }
}