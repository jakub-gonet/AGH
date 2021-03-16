#if !defined(BLOCK_MANAGER_H)
#define BLOCK_MANAGER_H

#include "vector.h"

struct BM_filename_pair {
  char* first;
  char* second;
};

typedef vec_type(vec_type(char)) BM_blocks;
typedef vec_type(struct BM_filename_pair) BM_pairs;

void BM_free_blocks(BM_blocks blocks);
void BM_free_pairs(BM_pairs pair);
BM_pairs BM_add_pair(BM_pairs pairs, char* first, char* second);
void BM_merge(struct BM_filename_pair pair);

#endif  // BLOCK_MANAGER_H
