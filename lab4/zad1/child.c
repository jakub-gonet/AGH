#include <assert.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#include "flag.h"
extern volatile sig_atomic_t signal_flag;

int str_to_signal(const char* string) {
  if (strcmp(string, "SIGHUP") == 0) {
    return SIGHUP;
  } else if (strcmp(string, "SIGINT") == 0) {
    return SIGINT;
  } else if (strcmp(string, "SIGQUIT") == 0) {
    return SIGQUIT;
  } else if (strcmp(string, "SIGILL") == 0) {
    return SIGILL;
  } else if (strcmp(string, "SIGABRT") == 0) {
    return SIGABRT;
  } else if (strcmp(string, "SIGFPE") == 0) {
    return SIGFPE;
  } else if (strcmp(string, "SIGKILL") == 0) {
    return SIGKILL;
  } else if (strcmp(string, "SIGSEGV") == 0) {
    return SIGSEGV;
  } else if (strcmp(string, "SIGPIPE") == 0) {
    return SIGPIPE;
  } else if (strcmp(string, "SIGALRM") == 0) {
    return SIGALRM;
  } else if (strcmp(string, "SIGTERM") == 0) {
    return SIGTERM;
  } else if (strcmp(string, "SIGCHLD") == 0) {
    return SIGCHLD;
  } else if (strcmp(string, "SIGSTOP") == 0) {
    return SIGSTOP;
  } else if (strcmp(string, "SIGCONT") == 0) {
    return SIGCONT;
  } else if (strcmp(string, "SIGTSTP") == 0) {
    return SIGTSTP;
  } else if (strcmp(string, "SIGTTIN") == 0) {
    return SIGTTIN;
  } else if (strcmp(string, "SIGTTOU") == 0) {
    return SIGTTOU;
  } else if (strcmp(string, "SIGUSR1") == 0) {
    return SIGUSR1;
  } else if (strcmp(string, "SIGUSR2") == 0) {
    return SIGUSR2;
  } else {
    assert(false && "no signal defined for this option.");
    exit(EXIT_FAILURE);
  }
}

void mask_signal(int signal_type) {
  sigset_t new_mask;
  sigemptyset(&new_mask);
  sigaddset(&new_mask, signal_type);
  sigprocmask(SIG_BLOCK, &new_mask, NULL);
}

void check_handlers(bool is_pending, const char* sig_name, int signal) {
  // sleep 100 ms
  usleep(100 * 1000);
  if (signal_flag) {
    printf("%s handled\n", sig_name);
  }

  if (is_pending) {
    sigset_t curr_mask;
    sigpending(&curr_mask);
    if (sigismember(&curr_mask, signal)) {
      printf("%s is pending.\n", sig_name);
    } else {
      printf("%s is NOT pending.\n", sig_name);
    }
  }

  if (!signal_flag) {
    printf("%s NOT handled\n", sig_name);
  }
}

int main(int argc, char const* argv[]) {
  assert(argc == 3);
  bool check_if_pending = false;
  const char* option = argv[1];
  const char* signal_type_str = argv[2];
  const int signal_type = str_to_signal(signal_type_str);
  if (strcmp(option, "ignore") == 0) {
    printf("=== Ignore ===\n");
    signal(signal_type, SIG_IGN);
  } else if (strcmp(option, "mask") == 0) {
    printf("=== Mask ===\n");
    mask_signal(signal_type);
  } else if (strcmp(option, "pending") == 0) {
    printf("=== Pending ===\n");
    mask_signal(signal_type);
    check_if_pending = true;
  } else {
    assert(false && "no action defined for this option.");
    exit(EXIT_FAILURE);
  }

  check_handlers(check_if_pending, signal_type_str, signal_flag);
  return 0;
}
