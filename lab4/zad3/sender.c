#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

volatile sig_atomic_t sig_flag = false;
volatile size_t sig_count = 0;
volatile sig_atomic_t catcher_sig_count = 0;

void handler(int sig_num, siginfo_t* info, void* ctx) {
  (void)ctx;
  if (sig_num == SIGUSR1 || sig_num == SIGRTMIN) {
    ++sig_count;
  } else if (sig_num == SIGUSR2 || sig_num == (SIGRTMIN + 1)) {
    sig_flag = true;
    if (info->si_code == SI_QUEUE) {
      printf("XD\n");
      catcher_sig_count = info->si_value.sival_int;
    }
  }
};

int main(int argc, char const* argv[]) {
  assert(argc == 3 + 1);
  const pid_t catcher_pid = strtol(argv[1], NULL, 10);
  assert(errno == 0);
  const unsigned long n = strtol(argv[2], NULL, 10);
  assert(errno == 0);
  const char* mode = argv[3];
  const bool via_sigqueue = strcmp(mode, "SIGQUEUE") == 0;
  const bool via_rt = strcmp(mode, "SIGRT") == 0;

  const int send_sig = via_rt ? SIGRTMIN : SIGUSR1;
  const int end_send_sig = via_rt ? SIGRTMIN + 1 : SIGUSR2;

  // block signals
  struct sigaction act;
  act.sa_sigaction = handler;
  act.sa_flags = SA_SIGINFO;

  sigset_t mask;
  sigfillset(&mask);
  sigdelset(&mask, SIGINT);
  sigdelset(&mask, send_sig);
  sigdelset(&mask, end_send_sig);
  sigprocmask(end_send_sig, &mask, NULL);
  // register signals
  sigaction(send_sig, &act, NULL);
  sigaction(end_send_sig, &act, NULL);

  // send signals
  for (size_t i = 0; i < n; ++i) {
    kill(catcher_pid, send_sig);
  }
  kill(catcher_pid, end_send_sig);

  // wait for signals
  while (!sig_flag) {
    sigsuspend(&mask);
  }

  printf("Sender got %ld signals, expected %ld signals\n", sig_count, n);
  if (via_sigqueue) {
    printf("Catcher received %d signals\n", catcher_sig_count);
  }
  return 0;
}
