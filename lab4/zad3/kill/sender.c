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

void handler(int sig_num) {
  if (sig_num == SIGUSR1) {
    ++sig_count;
  } else if (sig_num == SIGUSR2) {
    sig_flag = true;
  }
};

int main(int argc, char const* argv[]) {
  assert(argc == 3 + 1);
  const pid_t catcher_pid = strtol(argv[1], NULL, 10);
  assert(errno == 0);
  const unsigned long n = strtol(argv[2], NULL, 10);
  assert(errno == 0);
  const char* mode = argv[3];
  const bool is_rt = strcmp(mode, "SIGRT") == 0;

  const int send_sig = is_rt ? SIGRTMIN : SIGUSR1;
  const int end_send_sig = is_rt ? SIGRTMIN + 1 : SIGUSR2;

  // send signals
  for (size_t i = 0; i < n; ++i) {
    kill(catcher_pid, send_sig);
  }
  kill(catcher_pid, end_send_sig);

  // RECEIVE
  // block signals
  sigset_t mask;
  sigfillset(&mask);
  sigdelset(&mask, send_sig);
  sigdelset(&mask, SIGUSR2);
  sigprocmask(end_send_sig, &mask, NULL);
  // register signals
  signal(send_sig, handler);
  signal(end_send_sig, handler);

  // wait for signals
  while (!sig_flag) {
    sigsuspend(&mask);
  }

  printf("Sender got %ld signals, expected %ld signals\n", sig_count, n);

  return 0;
}
