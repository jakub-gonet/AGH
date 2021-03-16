#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vector.h"

bool try_extracting_file_pair(char **next_token, char **rest);

int main(int argc, char *argv[]) {
  for (int i = 1; i < argc; ++i) {
    const char *extracted = argv[i];
    printf("read: %s\n", extracted);
    if (!strcmp(extracted, "merge_files")) {
      printf("merge_files\n");
      char *next_token = NULL;
      char **current_s = &argv[++i];
      while (try_extracting_file_pair(&next_token, current_s)) {
        printf("first: %s, second: %s\n", next_token, *current_s);
        current_s = &argv[++i];
      }
      argv[i] = next_token;
      --i;
    } else if (!strcmp(extracted, "remove_block")) {
      const unsigned int index = strtol(argv[++i], NULL, 10);
      printf("remove_block\n");
    } else if (!strcmp(extracted, "remove_row")) {
      const unsigned int block_index = strtol(argv[++i], NULL, 10);
      const unsigned int row_index = strtol(argv[++i], NULL, 10);
      printf("remove_row %d %d\n", block_index, row_index);
    } else {
      printf("unrecognized: %s\n", extracted);
    }
  }

  return 0;
}

bool try_extracting_file_pair(char **next_token, char **rest) {
  *next_token = strsep(rest, ":");
  return *rest != NULL;
}