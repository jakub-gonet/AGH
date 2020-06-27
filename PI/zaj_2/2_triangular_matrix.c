/*
2. Macierze
1 Zadanie
Napisz program, który na wejściu otrzymuje macierz kwadratową i sprawdza, czy
jest ona trójkątna dolna. Macierz L jest dolnotrójkątna, jeżeli lij = 0 dla i <
j (czyli nad główną przekątną znajdują się elementy zerowe). 2 Wejście W
pierwszym wierszu standardowego wejścia znajduje się jedna liczba naturalna 1 ¬
n ¬ 100 reprezentująca rozmiar macierzy. Kolejne n wierszy zawiera po n liczb
naturalnych z zakresu [0, 1000] - wiersze macierzy. 3 Wyjście Na standardowym
wyjściu programu powinno znaleźć się jedno słowo - YES, jeśli dana macierz jest
trójkątna dolna, lub NO w przeciwnym przypadku.
*/
#include <stdio.h>
int main(void) {
  int size = 0;

  scanf("%d", &size);
  int number = 0;
  for (size_t i = 0; i < size; i++) {
    for (size_t j = 0; j < size; j++) {
      scanf("%d", &number);
      if (number != 0 && i < j) {
        printf("NO");
        return 0;
      }
    }
  }
  printf("YES");

  return 0;
}
