#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void sighandler(int sig_num, siginfo_t *info, void *ctx)
{
    printf("%d\n", info->si_value.sival_int);
}

int to_int(char *str)
{
    errno = 0;
    int val = (int)strtol(str, NULL, 10);
    assert(errno == 0);
    return val;
}

int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        printf("Not a suitable number of program parameters\n");
        return 1;
    }

    //..........

    //zablokuj wszystkie sygnaly za wyjatkiem SIGUSR1 i SIGUSR2
    //zdefiniuj obsluge SIGUSR1 i SIGUSR2 w taki sposob zeby proces potomny wydrukowal
    //na konsole przekazana przez rodzica wraz z sygnalami SIGUSR1 i SIGUSR2 wartosci

    sigset_t new_mask;
    sigfillset(&new_mask);
    sigdelset(&new_mask, SIGUSR1);
    sigdelset(&new_mask, SIGUSR2);
    sigprocmask(SIG_BLOCK, &new_mask, NULL);

    struct sigaction action;
    action.sa_flags = SA_SIGINFO;
    action.sa_sigaction = &sighandler;
    sigemptyset(&action.sa_mask);
    sigaction(SIGUSR1, &action, NULL);
    sigaction(SIGUSR2, &action, NULL);

    //---------
    int child = fork();
    if (child == 0)
    {
        sleep(1);
    }
    else
    {
        //wyslij do procesu potomnego sygnal przekazany jako argv[2]
        //wraz z wartoscia przekazana jako argv[1]
        union sigval sig_val;
        sig_val.sival_int = to_int(argv[1]);
        sigqueue(child, to_int(argv[2]), sig_val);
    }

    return 0;
}
