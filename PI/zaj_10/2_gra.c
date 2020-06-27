#include <stdio.h>
#define SIZE 100

int board[SIZE][SIZE] = {0};
int n, start_x, start_y, end_x, end_y;
int f(int x, int y) {
  if (board[y][x] == 0 || x > end_x || y > end_y) {
    return 0;
  }
  if (x == end_x && y == end_y) {
    return board[y][x];
  }

  return f(x + 1, y) + f(x, y + 1) + f(x + 1, y + 1);
}

int main(void) {
  scanf("%d", &n);
  scanf("%d %d", &start_x, &start_y);
  scanf("%d %d", &end_x, &end_y);

  for (int i = 0; i < n; i++) {
    for (int k = 0; k < n; k++) {
      scanf("%d", &board[i][k]);
    }
  }

  printf("%d", f(start_x, start_y));
}
