#include "block_manager.h"

#include <assert.h>
#include <stdio.h>

#include "vector.h"

void BM_add_pair(BM_pairs *const pairs, const char *const first,
                 const char *const second) {
  const struct BM_filename_pair new_pair = {.first = first, .second = second};
  vec_append(*pairs, new_pair);
}

BM_blocks BM_merge_pairs(BM_pairs pairs) {
  BM_blocks blocks = NULL;
  for (size_t i = 0; i < vec_get_size(pairs); i++) {
    printf("Processing %ld pair\n", i);
    FILE *merged_f = BM_merge_pair(&pairs[i]);
    const vec_type(char *) file_lines = NULL;

    char *line_buf = NULL;
    // unused
    size_t buf_size = 0;
    ssize_t line_len = 0;
    while ((line_len = getline(&line_buf, &buf_size, merged_f)) != -1) {
      vec_append(file_lines, line_buf);
      // make getline allocate a new buffer
      line_buf = NULL;
      buf_size = 0;
    }
    // last one needs to be freed
    free(line_buf);
    fclose(merged_f);
    vec_append(blocks, file_lines);
  }
  return blocks;
}

FILE *BM_merge_pair(const struct BM_filename_pair *const pair) {
  FILE *tmp_file = tmpfile();
  assert(tmp_file != NULL);

  FILE *file_a = fopen(pair->first, "r");
  assert(file_a != NULL);
  FILE *file_b = fopen(pair->second, "r");
  assert(file_b != NULL);

  // getline will allocate ourselves a buffor
  char *line_buf = NULL;
  size_t buf_size = 0;
  ssize_t line_len_a = -1;
  ssize_t line_len_b = -1;
  do {
    line_len_a = getline(&line_buf, &buf_size, file_a);
    if (line_len_a != -1) {
      fwrite(line_buf, sizeof(*line_buf), line_len_a, tmp_file);
    }

    line_len_b = getline(&line_buf, &buf_size, file_b);
    if (line_len_a != -1) {
      fwrite(line_buf, sizeof(*line_buf), line_len_b, tmp_file);
    }
  } while (line_len_a != -1 || line_len_b != -1);

  fclose(file_a);
  fclose(file_b);

  free(line_buf);

  rewind(tmp_file);
  return tmp_file;
}

void BM_free_blocks(BM_blocks blocks) {
  const size_t blocks_number = vec_get_size(blocks);
  for (size_t block_i = 0; block_i < blocks_number; ++block_i) {
    const size_t rows_number = vec_get_size(blocks[block_i]);
    for (size_t row_i = 0; row_i < rows_number; row_i++) {
      free((void *)blocks[block_i][row_i]);
    }
    vec_free(blocks[block_i]);
  }
  vec_free(blocks);
}

void BM_free_pairs(BM_pairs pairs) { vec_free(pairs); }