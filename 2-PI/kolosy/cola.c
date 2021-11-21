/*

1 Zadanie
Dana jest tablica liczb całkowitych, T o długości n. Napisz program, który na
wejściu otrzymuje wartości elementów tej tablicy i wyprowadza na standardowe
wyjście krotność pojawienia się każdej z wartości. Uwaga: W tym zadaniu używamy
dynamicznie alokowanej pamięci.

2 Wejście

W pierwszym wierszu standardowego wejścia znajduje się jedna liczba naturalna, 1
<= n <= 1000 będąca rozmiarem tablicy T. Kolejne n wierszy zawiera wartości
elementów tablicy T.

3 Wyjście

W pierwszym wierszu standardowego wyjścia znajduje się jedna liczba całkowita k
- liczba różnych wartości elementów tablicy T. Kolejne k wierszy powinny
zawierać po dwie liczby całkowite t i l, gdzie t to jedna z unikalnych wartości
tablicy T a l jest liczbą wystąpień wartości t w T. Wartości t powinny być
wypisane w kolejności ich pierwszego pojawienia się w tablicy T.

*/
#include <stdio.h>
#include <stdlib.h>

typedef struct count {
  int number;
  int occurences;
} count;

count* alloc(int k) { return malloc(k * sizeof(count)); }
void dealloc(count* map) { free(map); }

int main(void) {
  int k = 0;
  scanf("%d", &k);

  count* map = alloc(k);

  int num = 0;
  int last = -1;
  while (k--) {
    int changed = 0;

    scanf(" %d", &num);

    for (int i = 0; i <= last; ++i) {
      if (map[i].number == num) {
        map[i].occurences++;
        changed = 1;
        break;
      }
    }

    if (changed == 0) {
      map[++last] = (count){.number = num, .occurences = 1};
    }
  }

  printf("%d\n", last + 1);
  for (size_t i = 0; i <= last; ++i) {
    printf("%d %d\n", map[i].number, map[i].occurences);
  }

  dealloc(map);
}