/*
1 Zadanie
Dane są dwie tablice liczb całkowitych, T1 i T2, o długościach odpowiednio m i
n. Napisz program, który na wejściu otrzymuje wartości obu tablic i sortuje je
rosnąco. Następnie utwórz nową tablicę, T3, o długości co najwyżej m + n, która
powstaje przez połączenie tablic T1 i T2 w taki sposób, aby:
1. Zachować rosnące uporządkowanie wartości tablicy
2. Wartości tablicy T3 były unikalne
Uwaga: W tym zadaniu używamy dynamicznie alokowanej pamięci.
2 Wejście
W pierwszym wierszu standardowego wejścia znajdują się dwie liczby naturalne, 1
¬ m, n ¬ 1000 reprezentujące odpowiednio rozmiary tablic T1 i T2. Kolejne m + n
wierszy zawiera wartości tablic T1 oraz T2 (w tej kolejności).
3 Wyjście
Kolejne wiersze standardowego wyjścia programu powinny zawierać posortowane i
unikalne wartości tablicy T3.
*/

#include <stdio.h>
#include <stdlib.h>

// assuming that start <= n
int min_i_from_index(const int *const t, const int n, const int start) {
  int min = start;
  for (size_t i = start + 1; i < n; ++i) {
    if (t[i] < t[min]) {
      min = i;
    }
  }
  return min;
}

void swap(int *const t, const int a, const int b) {
  const int tmp = t[a];
  t[a] = t[b];
  t[b] = tmp;
}

void sort(int *t, int n) {
  // selection sort
  for (size_t i = 0; i < n; ++i) {
    // find element with minimal value
    const int min_i = min_i_from_index(t, n, i);
    // if not at current position, swap it
    if (min_i != i) {
      swap(t, i, min_i);
    }
  }
}

int merge(int *t1, int *t2, int *t3, int t1_size, int t2_size) {
  int t1_i = 0;
  int t2_i = 0;
  int t3_i = 0;
  while (t1_i < t1_size || t2_i < t2_size) {
    int next;
    // find next element to insert
    if (t1_i == t1_size) {
      next = t2[t2_i++];
    } else if (t2_i == t2_size) {
      next = t1[t1_i++];
    } else {
      // doesn't matter from what array we take if elements are equal
      next = t1[t1_i] <= t2[t2_i] ? t1[t1_i++] : t2[t2_i++];
    }

    // check for uniqueness
    if (t3_i == 0 || t3[t3_i - 1] != next) {
      t3[t3_i++] = next;
    }
  }
  return t3_i;
}

void print(int *t, int n) {
  for (size_t i = 0; i < n; i++) {
    printf("%d\n", t[i]);
  }
}

int err_code = 0;

int main() {
  int t1_size, t2_size;

  // assuming input is [1, 1000]
  scanf("%d %d", &t1_size, &t2_size);

  int *t1 = NULL;
  int *t2 = NULL;
  int *t3 = NULL;

  t1 = malloc(t1_size * sizeof(int));
  t2 = malloc(t2_size * sizeof(int));
  t3 = malloc((t1_size + t2_size) * sizeof(int));
  if (t1 == NULL || t2 == NULL || t3 == NULL) {
    err_code = 1;
    goto cleanup;
  }

  for (int i = 0; i < t1_size; i++) {
    scanf("%d", &t1[i]);
  }
  for (int i = 0; i < t2_size; i++) {
    scanf("%d", &t2[i]);
  }

  sort(t1, t1_size);
  sort(t2, t2_size);

  const int t3_size = merge(t1, t2, t3, t1_size, t2_size);

  print(t3, t3_size);
cleanup:
  free(t1);
  free(t2);
  free(t3);
  return err_code;
}