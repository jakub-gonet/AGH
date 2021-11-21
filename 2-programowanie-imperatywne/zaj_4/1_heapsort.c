/*
1. Heap sort

1 Zadanie

Napisz program, który posortuje liczby w ciąg niemalejący.
Program powinien stworzyć tablicę z wczytanych danych, a następnie posortować ją
algorytmem HeapSort.

2 Wejście

W pierwszym wierszu standardowego wejścia znajduje się jedna dodatnia liczba
całkowita Z oznaczająca liczbę zestawów danych do wczytania. Po niej następuje Z
zestawów danych który każdy składa się z: • wiersza z liczbą n • n wierszy z
liczbami do posortowania

3 Wyjście

Na standardowym wyjściu programu powinno znaleźć się Z zestawów po n wierszy
każdy z kolejnymi wyrazami posortowanych ciągów.

4 Przykład

4.1 Wejście

1
10
5
8
3
2
4
7
1
6
9
0

4.2 Wyjście

0
1
2
3
4
5
6
7
8
9
*/
#include <stdio.h>
#include <stdlib.h>

inline void swap(int *const arr, const int a, const int b) {
  const int tmp = arr[a];
  arr[a] = arr[b];
  arr[b] = tmp;
}

typedef struct heap {
  int *d;
  int size;
} heap;

inline int parent_i(const int i) { return (i - 1) / 2; }
inline int left_child_i(const int i) { return i * 2 + 1; }
inline int right_child_i(const int i) { return i * 2 + 2; }

void max_heapify(heap h, const int i) {
  const int left_i = left_child_i(i);
  const int right_i = right_child_i(i);
  int max_i = i;
  if (left_i < h.size && h.d[left_i] > h.d[max_i]) {
    max_i = left_i;
  }
  if (right_i < h.size && h.d[right_i] > h.d[max_i]) {
    max_i = right_i;
  }
  if (max_i != i) {
    swap(h.d, i, max_i);
    max_heapify(h, max_i);
  }
}

heap init_heap(int *const arr, const int n) {
  heap h = {.d = arr, .size = n};
  // last index storing node with leaves
  const int second_last_level = n / 2 + 1;

  for (int i = second_last_level; i >= 0; --i) {
    max_heapify(h, i);
  }
  return h;
}

void heapSort(int *arr, int n) {
  heap h = init_heap(arr, n);
  for (int i = h.size - 1; i >= 1; --i) {
    // insert max el to end
    swap(h.d, 0, i);
    // decrease size of heap by one: treat underlying array as smaller when
    // sorted elements grow from end
    --h.size;
    // heapify rest
    max_heapify(h, 0);
  }
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

    heapSort(array, n);

    for (int i = 0; i < n; ++i) {
      printf("%d\n", array[i]);
    }
    free(array);
  }
}
