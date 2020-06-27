/* 3. Liczby najpierwsze

1. Zadanie
Dana jest liczba całkowita dodatnia n.Napisz program,
który znajduje wszystkie liczby pierwsze mniejsze od n,
których cyfry tworzą ciąg niemalejący

2. Wejście
W pierwszym i jedynym
wierszy standardowego wejścia znajduje się jedna dodatnia liczba
naturalna n(n <= 10^8)

3. Wyjście
W kolejnych wierszach standardowego wyjścia powinny znaleźć się w porządku
rosnącym wszystkie liczby pierwsze, których cyfry tworzą ciąg niemalejący

4. Przykład

4.1 Wejście
100
4.2 Wyjście
2 3 5 7 11 13 17 19 23
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int* sieve_of_eratosthenes(const int up_to) {
  int* const sieve = calloc(up_to + 1, sizeof(int));
  if (sieve == NULL) {
    free(sieve);
    return NULL;
  }

  // initially all are primes
  for (size_t i = 2; i <= up_to; ++i) {
    sieve[i] = 1;
  }

  for (size_t i = 2; i <= sqrt(up_to); ++i) {
    if (sieve[i]) {
      for (size_t j = i * i; j <= up_to; j += i) {
        // remove multiples
        sieve[j] = 0;
      }
    }
  }

  return sieve;
}

const int has_non_decreasing_digits(int number) {
  while (number != 0) {
    const int curr_digit = number % 10;
    number /= 10;
    const int prev_digit = number % 10;
    if (!(prev_digit <= curr_digit)) {
      return 0;
    }
  }
  return 1;
}
int main(void) {
  int n = 0;
  scanf("%d", &n);
  int* primes = sieve_of_eratosthenes(n);
  if (primes == NULL) {
    return 1;
  }

  for (int i = 0; i < n; ++i) {
    if (primes[i] && has_non_decreasing_digits(i)) {
      printf("%d\n", i);
    }
  }

  free(primes);
  return 0;
}