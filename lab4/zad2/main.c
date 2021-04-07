#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
volatile sig_atomic_t reentry = false;
volatile sig_atomic_t oneshot = false;

void ext_handler(int sig, siginfo_t* info, void* ucontext) {
  (void)ucontext;
  if (info->si_pid == 0) {
    return;
  }
  if (reentry) {
    printf("[%d] SA_SIGINFO | SA_NODEFER, signal type: %d, reentry: %d\n",
           info->si_pid, sig, reentry);
    reentry = false;
    return;
  }
  printf("[%d] SA_SIGINFO %s, signal type: %d\n", info->si_pid,
         oneshot ? "| SA_RESETHAND" : "", sig);

  // signal_flag = true;
  // signal_type = sig;
  // sig_info = info;
  reentry = true;
  raise(SIGUSR1);
}

int main(void) {
  struct sigaction act;
  act.sa_sigaction = ext_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;
  sigaction(SIGUSR1, &act, NULL);

  raise(SIGUSR1);
  act.sa_sigaction = ext_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO | SA_NODEFER;
  sigaction(SIGUSR1, &act, NULL);

  raise(SIGUSR1);

  act.sa_sigaction = ext_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO | SA_RESETHAND;
  sigaction(SIGUSR1, &act, NULL);
  oneshot = true;
  raise(SIGUSR1);
  raise(SIGUSR1);

  return 0;
}
