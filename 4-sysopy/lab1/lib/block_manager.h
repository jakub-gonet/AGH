#if !defined(BLOCK_MANAGER_H)
#define BLOCK_MANAGER_H

#include <stdio.h>

#include "vector.h"

struct BM_filename_pair {
  const char* first;
  const char* second;
};
typedef const char* BM_row;
typedef vec_type(BM_row) BM_block;
typedef vec_type(BM_block) BM_blocks;

typedef vec_type(struct BM_filename_pair) BM_pairs;

void BM_add_pair(BM_pairs* const pairs, const char* const first,
                 const char* const second);
FILE* BM_merge_pair(const struct BM_filename_pair* const pair);
void BM_merge_pairs(BM_blocks* blocks, const BM_pairs pairs);

void BM_delete_block(BM_block* block);
void BM_delete_row(BM_row* row);

size_t BM_get_rows_count(const BM_blocks blocks, const size_t index);
void BM_print_blocks(const BM_blocks blocks);

void BM_free_blocks(BM_blocks blocks);
void BM_free_block(BM_block block);
void BM_free_row(BM_row row);
void BM_free_pairs(BM_pairs pair);

#endif  // BLOCK_MANAGER_H
