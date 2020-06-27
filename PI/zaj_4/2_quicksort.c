/*
1 Zadanie
Napisz program, który posortuje liczby w ciąg niemalejący.
Program powinien stworzyć tablicę z wczytanych danych, a następnie posortować ją
algorytmem QuickSort.

2 Wejście

W pierwszym wierszu standardowego wejścia znajduje się jedna dodatnia liczba
całkowita Z oznaczająca liczbę zestawów danych do wczytania. Po niej następuje Z
zestawów danych, który każdy składa się z: • wiersza z liczbą n, • n wierszy z
liczbami do posortowania.

3 Wyjście

Na standardowym wyjściu programu powinno znaleźć się Z zestawów po n wierszy
każdy z kolejnymi wyrazami posortowanych ciągów.
*/

#include <stdio.h>
#include <stdlib.h>

inline void swap(int *a, int *b) {
  const int tmp = *a;
  *a = *b;
  *b = tmp;
}

inline int get_pivot_i(start_i, end_i) { return end_i; }

int partition(int *const arr, const int start_i, const int end_i) {
  // keeping pivot in fixed position at end of hunk
  const int pivot_i = get_pivot_i(start_i, end_i);
  swap(&arr[pivot_i], &arr[end_i]);

  const int pivot = arr[end_i];

  int to_swap_i = start_i;
  for (int i = start_i; i <= end_i; ++i) {
    // we keep index of element which will be swapped if i-th element is less or
    // eq than pivot, which means that to_swap_i is index of number potentially
    // bigger than pivot. Elements left to to_swap_i are surely smaller than
    // pivot
    if (arr[i] < pivot) {
      swap(&arr[to_swap_i], &arr[i]);
      ++to_swap_i;
    }
  }

  swap(&arr[to_swap_i], &arr[end_i]);

  return to_swap_i;
}

void _quicksort(int *const arr, const int start_i, const int end_i) {
  if (start_i < end_i) {
    const int pivot_i = partition(arr, start_i, end_i);
    _quicksort(arr, start_i, pivot_i - 1);
    _quicksort(arr, pivot_i + 1, end_i);
  }
}
inline void quicksort(int *const arr, const int n) {
  _quicksort(arr, 0, n - 1);
}

int main() {
  int Z;
  scanf("%d", &Z);

  while (Z--) {
    int n;
    scanf("%d", &n);
    int *array = malloc(n * sizeof(int));
    for (int i = 0; i < n; ++i) {
      scanf("%d", &array[i]);
    }

    quicksort(array, n);

    for (int i = 0; i < n; ++i) {
      printf("%d\n", array[i]);
    }
    free(array);
  }
}