/*
1 Zadanie

Napisz program, który posortuje ciągi znaków reprezentujące liczby w ciąg
niemalejący. Program powinien stworzyć tablicę z wczytanych danych, a następnie
posortować ją pozycyjnie.

2 Wejście

W pierwszym wierszu standardowego wejścia znajduje się jedna dodatnia liczba
całkowita Z oznaczająca liczbę zestawów danych do wczytania. Po niej następuje Z
zestawów danych. Każdy z zestawów składa się z: • wiersza z liczbami n i k, • n
wierszy z liczbami do posortowania, przy czym każda z tych liczb jest być liczbą
naturalną k-cyfrową.

3 Wyjście

Na standardowym wyjściu programu powinno znaleźć się Z zestawów po n wierszy
każdy z kolejnymi wyrazami posortowanych ciągów.
*/

#include <stdio.h>
#include <stdlib.h>

int** alloc(const int n, const int k) {
  int** to_sort = malloc(n * sizeof(int*));

  for (int i = 0; i < n; ++i) {
    to_sort[i] = malloc(k * sizeof(int));
  }

  return to_sort;
}

void dealloc(int** arr, const int n) {
  for (int i = 0; i < n; i++) {
    free(arr[i]);
  }
  free(arr);
}

void read_data(int** array, const int n, const int k) {
  for (int col = 0; col < n; ++col) {
    for (int row = 0; row < k; ++row) {
      char digit;
      scanf(" %c", &digit);
      array[col][row] = digit - '0';
    }
  }
}

void print_data(int** const array, const int n, const int k) {
  for (int col = 0; col < n; ++col) {
    for (int row = 0; row < k; ++row) {
      printf("%d", array[col][row]);
    }
    printf("\n");
  }
}

void partial_sums(int* array, const int size) {
  for (int i = 1; i < size; ++i) {
    array[i] = array[i - 1] + array[i];
  }
}

#define COUNT_SIZE 10
void count_sort(int** array, const int n, const int k) {
  int** sorted = malloc(n * sizeof(int*));
  int count[COUNT_SIZE] = {0};

  for (int i = 0; i < n; ++i) {
    // count digits in k-th column
    ++count[array[i][k]];
  }

  partial_sums(count, COUNT_SIZE);

  for (int i = n - 1; i >= 0; --i) {
    --count[array[i][k]];
    sorted[count[array[i][k]]] = array[i];
  }

  for (int i = 0; i < n; ++i) {
    array[i] = sorted[i];
  }

  free(sorted);
}

void radix_sort(int** array, const int n, int k) {
  while (k--) {
    count_sort(array, n, k);
  }
}

int main(void) {
  int Z;
  scanf("%d", &Z);

  int** array = NULL;
  int n = 0, k = 0;
  while (Z--) {
    scanf("%d %d", &n, &k);
    array = alloc(n, k);
    read_data(array, n, k);
    radix_sort(array, n, k);
    print_data(array, n, k);
    dealloc(array, n);
  }
}