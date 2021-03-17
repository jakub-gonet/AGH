#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "block_manager.h"
#include "vector.h"

bool try_extracting_file_pair(char **next_token, char **rest);

int main(int argc, char *argv[]) {
  BM_pairs filename_pairs = NULL;

  for (int i = 1; i < argc; ++i) {
    const char *extracted = argv[i];
    printf("read: %s\n", extracted);
    if (!strcmp(extracted, "merge_files")) {
      printf("merge_files\n");
      char *next_token = NULL;
      char **current_s = &argv[++i];
      while (try_extracting_file_pair(&next_token, current_s)) {
        // *move* ptrs to pairs
        BM_add_pair(&filename_pairs, next_token, *current_s);
        current_s = &argv[++i];
      }
      BM_blocks blocks = BM_merge_pairs(filename_pairs);
      for (size_t i = 0; i < vec_get_size(blocks); i++) {
        printf("%ld row size\n", vec_get_size(blocks[i]));
        for (size_t j = 0; j < vec_get_size(blocks[i]); j++) {
          printf("%ld block, %ld row: %s", i, j, blocks[i][j]);
        }
        printf("\n");
      }

      BM_free_blocks(blocks);

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

  BM_free_pairs(filename_pairs);
  return 0;
}

bool try_extracting_file_pair(char **next_token, char **rest) {
  *next_token = strsep(rest, ":");
  return *rest != NULL;
}