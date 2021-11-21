#if !defined(FLAG_H)
#define FLAG_H

#include <signal.h>

volatile sig_atomic_t signal_flag = false;

#endif  // FLAG_H
