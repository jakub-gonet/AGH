#include <stdio.h>
#include <stdlib.h>

int count(int** arr, int n, int x, int y) {
  int direction_x[] = {-1, 0, 1, 1, 1, 0, -1, -1};
  int direction_y[] = {-1, -1, -1, 0, 1, 1, 1, 0};
  int sum = 0;
  for (int i = 0; i < 8; i++) {
    int new_x = x + direction_x[i];
    int new_y = y + direction_y[i];
    if (new_x < 0 || new_x >= n || new_y < 0 || new_y >= n) {
      continue;
    }
    sum += arr[new_x][new_y];
  }
  return sum;
}

int main(void) {
  int n;
  scanf("%d", &n);
  int** arr = malloc(n * sizeof(int*));
  for (int i = 0; i < n; i++) {
    arr[i] = malloc(n * sizeof(int));
  }

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      scanf(" %d", &arr[i][j]);
    }
  }

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      printf("%d ", count(arr, n, i, j));
    }
    printf("\n");
  }

  for (int i = 0; i < n; i++) {
    free(arr[i]);
  }
  free(arr);
}