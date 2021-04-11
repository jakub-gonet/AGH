#include <assert.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

volatile sig_atomic_t sig_flag = false;
volatile size_t sig_count = 0;
volatile sig_atomic_t sender_pid = 0;
volatile sig_atomic_t sig_type_send = SIGUSR1;
volatile sig_atomic_t sig_type_end = SIGUSR2;

void handler(int sig_num, siginfo_t* info, void* ucontext) {
  (void)ucontext;
  sender_pid = info->si_pid;
  if (sig_num == SIGUSR1 || sig_num == SIGRTMIN) {
    ++sig_count;
    sig_type_send = sig_num;
  } else if (sig_num == SIGUSR2 || sig_num == (SIGRTMIN + 1)) {
    sig_flag = true;
    sig_type_end = sig_num;
  }
};

int main(void) {
  // print pid
  printf("%d\n", getpid());

  // block signals
  struct sigaction act;
  act.sa_sigaction = handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = SA_SIGINFO;
  sigset_t mask;
  sigfillset(&mask);
  sigdelset(&mask, SIGINT);
  sigdelset(&mask, SIGUSR1);
  sigdelset(&mask, SIGUSR2);
  sigdelset(&mask, SIGRTMIN);
  sigdelset(&mask, SIGRTMIN + 1);
  act.sa_mask = mask;
  sigprocmask(SIG_SETMASK, &mask, NULL);
  // register signals
  sigaction(SIGUSR1, &act, NULL);
  sigaction(SIGUSR2, &act, NULL);
  sigaction(SIGRTMIN, &act, NULL);
  sigaction(SIGRTMIN + 1, &act, NULL);

  // wait for signals
  while (!sig_flag) {
    sigsuspend(&mask);
  }

  // flag is set, lets echo signals
  for (size_t i = 0; i < sig_count; ++i) {
    kill(sender_pid, sig_type_send);
  }
  sigqueue(sender_pid, sig_type_end, (union sigval){sig_count});

  printf("Catcher got %ld signals\n", sig_count);

  return 0;
}
