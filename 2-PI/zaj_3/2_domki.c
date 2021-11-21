/*
2. Równe połówki

1 Zadanie
Napisz program, który przyjmuję tablicę liczb naturalnych i zwraca taki indeks,
że sumy wartości elementów tablicy na lewo i na prawo od wyznaczonego miejsca są
równe. Można założyć, że rozwiązanie istnieje.

2 Wejście
W pierwszym wierszu standardowego wejścia znajduje się jedna dodatnia liczba
całkowita 1 ¬ n ¬ 1000 oznaczająca liczbę elementów tablicy. Kolejny wiersz
zawiera dokładnie n liczb całkowitych - elementy tablicy.

3 Wyjście
Na standardowym wyjściu programu powinna znaleźć się jedna liczba - indeks
elementu dzielącego tablicę na dwie części o równych sumach.

4 Przykład
4.1 Wejście
5
6 7 3 2 1
4.2 Wyjście
1
*/
#include <stdio.h>

#define N 1000

void partial_sums(int array[], const int size) {
  for (size_t i = 1; i < size; ++i) {
    array[i] = array[i - 1] + array[i];
  }
}

int find_index_with_eq_sum(int array[], const int size) {
  partial_sums(array, size);

  for (size_t i = 0; i < size; ++i) {
    if ((i >= 1 ? array[i - 1] : 0) == (array[size - 1] - array[i])) {
      return i;
    }
  }

  return -1;
}

int main(void) {
  int array[N];
  int size = 0;

  scanf("%d", &size);
  for (size_t i = 0; i < size; ++i) {
    scanf("%d", &array[i]);
  }

  const int index = find_index_with_eq_sum(array, size);
  printf("%d", index);
  return 0;
}