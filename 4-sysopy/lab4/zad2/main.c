#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
volatile sig_atomic_t reentry = false;
volatile sig_atomic_t oneshot = false;

void ext_handler(int sig, siginfo_t* info, void* ucontext) {
  (void)ucontext;
  if (reentry) {
    printf("[%d] SA_SIGINFO | SA_NODEFER, signal type: %d, reentry: %d\n",
           info->si_pid, sig, reentry);
    reentry = false;
    return;
  }
  printf("[%d] SA_SIGINFO %s, signal type: %d\n", info->si_pid,
         oneshot ? "| SA_RESETHAND" : "", sig);

  reentry = true;
  printf("Before raise\n");
  raise(SIGUSR1);
  printf("After raise\n");
}

int main(void) {
  struct sigaction act;
  act.sa_sigaction = ext_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;
  sigaction(SIGUSR1, &act, NULL);
  printf("== SA_SIGINFO\n");
  raise(SIGUSR1);
  act.sa_sigaction = ext_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO | SA_NODEFER;
  sigaction(SIGUSR1, &act, NULL);

  printf("== SA_SIGINFO | SA_NODEFER\n");
  raise(SIGUSR1);

  act.sa_sigaction = ext_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO | SA_RESETHAND;
  sigaction(SIGUSR1, &act, NULL);
  oneshot = true;
  printf("== SA_SIGINFO | SA_RESETHAND\n");

  raise(SIGUSR1);
  raise(SIGUSR1);

  return 0;
}
