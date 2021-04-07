// Napisz program demonstrujący, czy ustawienia dyspozycji dla sygnałów, ich
// maski oraz czekające sysgnały są dziedziczone po wykonaniu funkcji fork oraz
// exec.

// W szczególności eksperymenty proszę wykonać dla sygnału SIGUSR1 w
// następujący sposób:

// - Dziedziczenie ustawień sygnałów po wykonaniu funkcji fork.
// Proszę napisać program, który w zależności od wartości argumentu z linii
// poleceń, który może przyjmować wartości ignore, handler, mask lub pending,
//  - odpowiednio w procesie przodka ustawia ignorowanie,
//  - instaluje handler  obsługujący sygnał wypisujący komunikat o jego
//  otrzymaniu,
//  - maskuje ten sygnał
//  - oraz sprawdza (przy zamaskowaniu tego sygnału) czy wiszący/oczekujący
//  sygnał jest widoczny w procesie,
//    a następnie przy pomocy funkcji raise wysyła sygnał do samego siebie oraz
// wykonuje odpowiednie dla danej opcji działania,
//    po czym tworzy potomka funkcją fork i ponownie przy pomocy funkcji raise
//    potomek wysyła sygnał do samego siebie
// (z wyjątkiem opcji pending, gdzie testowane jest sprawdzenie, czy sygnał
// czekający w przodku jest widoczny w potomku).

// - Dziedziczenie ustawień sygnałów po wykonaniu
// funkcji exec. W podobny sposób sprawdź jaki wpływ na ustawienia sygnałów
// ma wywołanie funkcji exec. Rozpatrz opcje:  ignore, mask i pending.
// Przygotuj plik raport2.txt w którym nastąpi podsumowanie z wnioskami z
// wykonanych powyższych eksperymentów
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

volatile sig_atomic_t signal_flag = false;

void signal_handler(int sig_num) {
  (void)sig_num;
  signal_flag = true;
}

void mask_signal(int signal_type) {
  sigset_t new_mask;
  sigemptyset(&new_mask);
  sigaddset(&new_mask, signal_type);
  sigprocmask(SIG_BLOCK, &new_mask, NULL);
}

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

int main(int argc, char const* argv[]) {
  assert(argc == 3);
  bool check_if_pending = false;
  const char* option = argv[1];
  const char* signal_type_str = argv[2];
  const int signal_type = str_to_signal(signal_type_str);
  if (strcmp(option, "ignore") == 0) {
    printf("=== Ignore ===\n");
    signal(signal_type, SIG_IGN);
  } else if (strcmp(option, "handler") == 0) {
    printf("=== Handler ===\n");
    signal(signal_type, signal_handler);
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
  fflush(stdout);
  pid_t pid = fork();
  assert(pid >= 0);
  if (pid == 0) {
    printf("====== Child process ======\n");
  } else {
    wait(NULL);
    printf("====== Parent process ======\n");
  }

  raise(signal_type);

  time_t curr_time = time(NULL);
  time_t end_wait = curr_time + 1;
  // wait 1s
  while (curr_time < end_wait) {
    curr_time = time(NULL);
    // sleep 100 ms
    usleep(100 * 1000);

    if (signal_flag) {
      printf("%s handled\n", signal_type_str);
      break;
    }
  }

  if (check_if_pending) {
    sigset_t curr_mask;
    sigpending(&curr_mask);
    if (sigismember(&curr_mask, signal_type)) {
      printf("%s is pending.\n", signal_type_str);
    } else {
      printf("%s is NOT pending.\n", signal_type_str);
    }
  }

  if (!signal_flag) {
    printf("%s NOT handled\n", signal_type_str);
  }
  if (pid > 0) {
    printf("\n");
  }
  return 0;
}
