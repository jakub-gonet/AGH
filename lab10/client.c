#include <stdio.h>

#include "common.h"

void print_board(area_t area) {
  printf("+-----------+\n");
  for (size_t y = 0; y < GAME_SIZE; y++) {
    for (size_t x = 0; x < GAME_SIZE; x++) {
      enum cell_type c = area[y][x];
      printf("| %s ", c == _ ? " " : c == X ? "X" : c == O ? "O" : "$");
    }
    printf("|\n");
    if (y != GAME_SIZE - 1) {
      printf("+---+---+---+\n");
    }
  }
  printf("+-----------+\n");
}

int main(int argc, char const* argv[]) {
  return 0;
}
