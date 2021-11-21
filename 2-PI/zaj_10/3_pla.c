#include <stdio.h>
#include <string.h>
#define SIZE 201
#define CACHE_SIZE (SIZE + 1)
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MIN_3(a, b, c) (MIN(MIN((a), (b)), (c)))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

int cache[CACHE_SIZE][CACHE_SIZE];

int levenshtein(char* a, char* b, int i, int j) {
  if (MIN(i, j) == 0) {
    return MAX(i, j);
  } else {
    if (cache[i - 1][j] == -1) {
      cache[i - 1][j] = levenshtein(a, b, i - 1, j);
    }
    if (cache[i][j - 1] == -1) {
      cache[i][j - 1] = levenshtein(a, b, i, j - 1);
    }
    if (cache[i - 1][j - 1] == -1) {
      cache[i - 1][j - 1] = levenshtein(a, b, i - 1, j - 1);
    }

    return MIN_3(cache[i - 1][j] + 1, cache[i][j - 1] + 1,
                 cache[i - 1][j - 1] + (a[i - 1] != b[j - 1]));
  }
}

int main(void) {
  for (int i = 0; i < CACHE_SIZE; i++) {
    for (int j = 0; j < CACHE_SIZE; j++) {
      cache[i][j] = -1;
    }
  }

  char buf_a[SIZE];
  char buf_b[SIZE];

  scanf("%[^\n]", buf_a);
  getchar();
  scanf("%[^\n]", buf_b);

  printf("%d\n", levenshtein(buf_a, buf_b, strlen(buf_a), strlen(buf_b)));
}
