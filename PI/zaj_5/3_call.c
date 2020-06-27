/*
3. Call center

1 Zadanie

Napisz program, który symuluje działanie call center firmy usługowej. Na wejściu
pojawiają się zgłoszenia klientów oraz informacje o możliwości odebrania
kolejnego połączenia. Połączenia powinny być odbierane w kolejności zgłoszeń
(FIFO).

2 Wejście

W pierwszym wierszu standardowego wejścia znajduje się jedna dodatnia liczba
całkowita n oznaczająca liczbę operacji. Po niej następuje n wierszy z
operacjami. Każda operacja ma format: <typ> <sekundy> [numer] Możliwe typy
operacji:
• a [add] - dodaj numer telefonu do kolejki połączeń oczekujących
• r [remove] - usuń pierwszy numer z kolejki (połączenie odebrane) i wydrukuj
informację o połączeniu (numer telefonu i czas oczekiwania).
Numer telefonu jest stringiem nie dłuższym niż 15 znaków. Można założyć, że
wartość pola <sekundy> dla kolejnych operacji jest ciągiem niemalejącym.

3 Wyjście

Standardowe wyjście programu powinno tyle wierszy ile odebrano połączeń
(operacje r). Każdy wiersz składa się z numeru telefonu i (po spacji) czasu
oczekiwania na połączenie. Czas oczekiwania jest obliczany jako różnica pól
<sekundy> podanych w operacji r i operacji a dla tego numeru.

4 Przykład
4.1 Wejście

6
a 12 555222333
a 24 777555333
r 27
a 31 444333999
r 45
r 56

4.2 Wyjście

555222333 15
777555333 21
444333999 25

*/

#include <stdio.h>
#include <stdlib.h>
#define TEL_N_LENGTH 15

typedef struct entry {
  char tel_no[TEL_N_LENGTH + 1];
  unsigned int timestamp;
} entry;

typedef struct queue {
  entry *data;
  unsigned int start;
  unsigned int end;
} queue;

const int calc_time_diff(const unsigned int timestamp,
                         const unsigned int curr_time) {
  return curr_time - timestamp;
}

void push_back(queue *const q, const entry e) { q->data[q->end++] = e; }

const entry pop_front(queue *const q) { return q->data[q->start++]; }

queue alloc_queue(const int n) {
  return (queue){.start = 0, .end = 0, .data = malloc(n * sizeof(entry))};
}

void dealloc_queue(queue *q) { free(q->data); };

void read_and_add_entry(queue *q) {
  entry e;
  scanf(" %d %s", &e.timestamp, e.tel_no);
  push_back(q, e);
}

void remove_and_print_entry(queue *q) {
  unsigned int curr_time;
  scanf(" %d", &curr_time);
  const entry e = pop_front(q);
  printf("%s %d\n", e.tel_no, calc_time_diff(e.timestamp, curr_time));
}

int main(void) {
  unsigned int n;
  scanf("%d", &n);

  queue q = alloc_queue(n);

  while (n--) {
    char op;
    scanf(" %c", &op);
    if (op == 'a') {
      read_and_add_entry(&q);
    } else if (op == 'r') {
      remove_and_print_entry(&q);
    }
  }

  dealloc_queue(&q);
}