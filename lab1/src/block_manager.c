#include "block_manager.h"

#include "vector.h"

void BM_free(BM_blocks blocks) {
  const size_t size = vec_get_size(blocks);
  for (size_t i = 0; i < size; ++i) {
    vec_free(blocks[i]);
  }
  vec_free(blocks);
}

BM_pairs BM_add_pair(BM_pairs pairs, char *first, char *second) {
  struct BM_filename_pair new_pair = {.first = first, .second = second};
  vec_append(pairs, new_pair);
}
void BM_merge(struct BM_filename_pair pair) {}

void BM_free_pairs(BM_pairs pairs) { vec_free(pairs); }