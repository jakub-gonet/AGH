#if !defined(BLOCK_MANAGER_H)
#define BLOCK_MANAGER_H

#include <stdio.h>

#include "vector.h"

struct BM_filename_pair {
  const char* first;
  const char* second;
};

typedef const vec_type(vec_type(char*)) BM_blocks;
typedef vec_type(struct BM_filename_pair) BM_pairs;

void BM_free_blocks(BM_blocks blocks);
void BM_free_pairs(BM_pairs pair);
void BM_add_pair(BM_pairs* const pairs, const char* const first,
                 const char* const second);
BM_blocks BM_merge_pairs(BM_pairs pairs);
FILE* BM_merge_pair(const struct BM_filename_pair* const pair);

#endif  // BLOCK_MANAGER_H
