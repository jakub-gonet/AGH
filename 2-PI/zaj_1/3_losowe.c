/*
Program losuje liczbę 0 ≤ X ≤ 100.
Napisz funkcję, która zgaduje wartość X.
W pętli losujemy n∈[0,100]. Jeżeli X=n zgadliśmy X,
jeżeli nie na podstawie wartości X i n ograniczamy przedział, z którego losujemy
kolejne n.
*/
#define _XOPEN_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

const int gen(const int a, const int b) {
  return (int)((b - (double)a) * drand48() + a + 0.5);
}

const int FROM = 0;
const int TO = 100;

int main(void) {
  srand48(time(NULL));

  const int to_guess = gen(FROM, TO);
  printf("To guess: %d\n", to_guess);

  int range_start = FROM;
  int range_end = TO;
  int guesses_count = 0;

  while (1) {
    const int guess = gen(range_start, range_end);
    ++guesses_count;

    if (to_guess == guess) {
      printf("Guessed number! Number: %d, number of guesses: %d\n", to_guess,
             guesses_count);
      return 0;
    } else if (to_guess < guess) {
      range_start = to_guess;
    } else {
      range_end = to_guess;
    }
  }
}