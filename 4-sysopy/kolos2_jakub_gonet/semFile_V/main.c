#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define FILE_NAME "common.txt"
#define SEM_NAME "/kol_sem"

void obtain(int semid) {
  struct sembuf sem_op = {.sem_num = 0, .sem_op = -1, .sem_flg = 0};
  int res = semop(semid, &sem_op, 1);
}

void release(int semid) {
  struct sembuf sem_op = {.sem_num = 0, .sem_op = 1, .sem_flg = 0};
  int res = semop(semid, &sem_op, 1);
}

int main(int argc, char** args) {
  if (argc != 4) {
    printf("Not a suitable number of program parameters\n");
    return (1);
  }

  /**************************************************
  Stworz semafor systemu V
  Ustaw jego wartosc na 1
  ***************************************************/

  union semun {
    int val;
    struct semid_ds* buf;
    unsigned short* array;
    struct seminfo* __buf;
  } semun;
  semun.val = 1;
  int sem_id = semget(ftok(SEM_NAME, 'x'), 1, IPC_CREAT | IPC_EXCL | 0666);
  semctl(sem_id, 0, SETVAL, semun);

  int fd = open(FILE_NAME, O_WRONLY | O_CREAT | O_TRUNC, 0644);

  int parentLoopCounter = atoi(args[1]);
  int childLoopCounter = atoi(args[2]);

  char buf[20];
  pid_t childPid;
  int max_sleep_time = atoi(args[3]);

  if (childPid = fork()) {
    int status = 0;
    srand((unsigned)time(0));

    while (parentLoopCounter--) {
      int s = rand() % max_sleep_time + 1;
      sleep(s);

      /*****************************************
      sekcja krytyczna zabezpiecz dostep semaforem
      **********************************************/

      obtain(sem_id);
      sprintf(buf, "Wpis rodzica. Petla %d. Spalem %d\n", parentLoopCounter, s);
      write(fd, buf, strlen(buf));
      write(1, buf, strlen(buf));
      release(sem_id);
      /*********************************
      Koniec sekcji krytycznej
      **********************************/
    }
    waitpid(childPid, &status, 0);
  } else {
    srand((unsigned)time(0));
    while (childLoopCounter--) {
      int s = rand() % max_sleep_time + 1;
      sleep(s);

      /*****************************************
      sekcja krytyczna zabezpiecz dostep semaforem
      **********************************************/
      obtain(sem_id);
      sprintf(buf, "Wpis dziecka. Petla %d. Spalem %d\n", childLoopCounter, s);
      write(fd, buf, strlen(buf));
      write(1, buf, strlen(buf));
      release(sem_id);

      /*********************************
      Koniec sekcji krytycznej
      **********************************/
    }
    _exit(0);
  }

  /*****************************
  posprzataj semafor
  ******************************/
  semctl(sem_id, -1, IPC_RMID);
  close(fd);
  return 0;
}
