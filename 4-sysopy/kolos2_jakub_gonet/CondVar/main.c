#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t var = PTHREAD_COND_INITIALIZER;

int count = 0;

/*
 * Funkcja 'wait_for_counter' powinna czekać, aż zmienna globalna 'count'
 * chroniona przez mutex 'mutex' osiągnie wartość co najmniej 1000, a następnie
 * zwiększyć ją o 10.
 */

void* wait_for_counter(void* arg) {
  pthread_mutex_lock(&mutex);
  while (count < 1000) {
    pthread_cond_wait(&var, &mutex);
  }
  printf("Changing value: %d\n", count);
  count += 10;
  pthread_mutex_unlock(&mutex);
  pthread_exit(NULL);
}

int main(void) {
  for (int k = 0; k < 100; ++k) {
    count = 0;

    pthread_t tid;
    /* Stwórz nowy wątek wykonujący funkcje wait_for_counter.
     */
    pthread_create(&tid, NULL, &wait_for_counter, NULL);
    int reps = 100000;
    for (int i = 0; i < reps; ++i) {
      pthread_mutex_lock(&mutex);
      ++count;
      pthread_cond_signal(&var);
      pthread_mutex_unlock(&mutex);
    }
    /* Poczekaj na zakończenie stworzonego wątku.
     */
    pthread_join(tid, NULL);
    pthread_mutex_lock(&mutex);
    if (count != reps + 10) {
      printf("Wrong value: %d\n", count);
      exit(-1);
    }
    pthread_mutex_unlock(&mutex);
  }
  puts("OK");
}
