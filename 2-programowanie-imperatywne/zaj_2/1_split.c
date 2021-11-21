/* 1 Zadanie
Dana jest tablica liczb całkowitych, T1, o długości n. Napisz program, który na
wejściu otrzymuje wartości T1 i dzieli ją na dwie tablice, T2 i T3, tak, by w
jednej z nich znalazły się elementy parzyste a w drugiej nieparzyste. Następnie
posortuj obie tablice.
2 Wejście
W pierwszym wierszu standardowego wejścia
znajduje się liczba naturalna, n (z zakresu [1, 1000]) reprezentująca rozmiar
tablicy T1. Kolejne n wierszy zawiera wartości tablicy T1.
3 Wyjście
Pierwszy wiersz standardowego wyjścia programu powinien zawierać dwie liczby
całkowite e i o (odpowiednio długości tablicy T2 elementów parzystych i tablicy
T3 elementów nieparzystych) a następnie e wierszy z wartościami T2 posortowanymi
rosnąco i o wierszy z wartościami T3 również posortowanymi rosnąco.
*/
#include <stdio.h>
#include <stdlib.h>
int err_code = 0;
void sort(int* t, int n) {
  // insertion sort
  // 1 el array is sorted from definition
  for (size_t i = 1; i < n; ++i) {
    // save i-th element
    const int x = t[i];
    // start on one element before it
    int j = i - 1;
    // while in bounds and j-th > x
    while (j >= 0 && t[j] > x) {
      // find place for x - element which is smaller than x
      t[j + 1] = t[j];
      --j;
    }
    // insert x at element next to el smaller than x
    t[j + 1] = x;
  }
}

int split(int* t1, int* t2, int* t3, int n) {
  size_t t2_i = 0;
  size_t t3_i = 0;
  for (size_t i = 0; i < n; ++i) {
    const int item = t1[i];
    if (item % 2 == 0) {
      t2[t2_i++] = item;
    } else {
      t3[t3_i++] = item;
    }
  }

  return t2_i;
}

void print(int* t, int n) {
  for (int i = 0; i < n; i++) {
    printf("%d\n", t[i]);
  }
}
int main() {
  int n;
  // n is [1, 1000], we don't need to check n=0 case
  scanf("%d", &n);

  int* t1 = NULL;
  int* t2 = NULL;
  int* t3 = NULL;

  t1 = malloc(n * sizeof(int));
  t2 = malloc(n * sizeof(int));
  t3 = malloc(n * sizeof(int));
  if (t1 == NULL || t2 == NULL || t3 == NULL) {
    err_code = 1;
    goto cleanup;
  }

  for (int i = 0; i < n; i++) {
    scanf("%d", &t1[i]);
  }

  const int e = split(t1, t2, t3, n);
  const int o = n - e;

  sort(t2, e);
  sort(t3, o);

  printf("%d %d\n", e, o);
  print(t2, e);
  print(t3, o);

cleanup:
  free(t1);
  free(t2);
  free(t3);
  return err_code;
}
