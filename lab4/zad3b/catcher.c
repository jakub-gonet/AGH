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
volatile sig_atomic_t sig_ignore = false;

void handler(int sig_num, siginfo_t* info, void* ucontext) {
  (void)ucontext;
  if (sig_ignore) {
    return;
  }
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
  sigset_t block_all;
  sigfillset(&block_all);
  sigdelset(&block_all, SIGINT);
  sigprocmask(SIG_SETMASK, &block_all, NULL);

  struct sigaction act;
  act.sa_sigaction = handler;
  act.sa_flags = SA_SIGINFO;
  sigemptyset(&act.sa_mask);

  sigset_t mask;
  sigdelset(&mask, SIGUSR1);
  sigdelset(&mask, SIGUSR2);
  sigdelset(&mask, SIGRTMIN);
  sigdelset(&mask, SIGRTMIN + 1);
  // register signals
  sigaction(SIGUSR1, &act, NULL);
  sigaction(SIGUSR2, &act, NULL);
  sigaction(SIGRTMIN, &act, NULL);
  sigaction(SIGRTMIN + 1, &act, NULL);

  // wait for signals
  while (!sig_flag) {
    sigsuspend(&mask);
    kill(sender_pid, sig_type_send);
    usleep(100);
  }
  // ignore signals
  sig_ignore = true;

  // flag is set, lets echo signals
  for (size_t i = 0; i < sig_count; ++i) {
    kill(sender_pid, sig_type_send);
    sigsuspend(&mask);
  }
  sigqueue(sender_pid, sig_type_end, (union sigval){sig_count});

  printf("Catcher got %ld signals\n", sig_count);

  return 0;
}
