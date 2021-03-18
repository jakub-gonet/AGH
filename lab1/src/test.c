#include <assert.h>
#include <bm/block_manager.h>
#include <bm/vector.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/times.h>

// #include "../lib/block_manager.h"
// #include "../lib/vector.h"

#ifdef DYNAMIC
#include <dlfcn.h>
#endif

#define MEASURE(MSG, STATEMENT)                                          \
  do {                                                                   \
    /* TIME START */                                                     \
    struct tms tms_measure_start;                                        \
    clock_t real_measure_start = times(&tms_measure_start);              \
                                                                         \
    STATEMENT                                                            \
                                                                         \
    /* TIME END */                                                       \
    struct tms tms_measure_end;                                          \
    clock_t real_measure_end = times(&tms_measure_end);                  \
    clock_t real_elapsed = real_measure_end - real_measure_start;        \
    clock_t user_elapsed =                                               \
        tms_measure_end.tms_utime - tms_measure_start.tms_utime;         \
    clock_t system_elapsed =                                             \
        tms_measure_end.tms_stime - tms_measure_start.tms_stime;         \
    printf(MSG " real, user, system\n");                                 \
    printf("%ld %ld %ld\n", real_elapsed, user_elapsed, system_elapsed); \
  } while (0);

#define LIST_BM_FUNCTIONS(TRANSFORM_SIGNATURE)                           \
  TRANSFORM_SIGNATURE(void, BM_add_pair, BM_pairs *const pairs,          \
                      const char *const first, const char *const second) \
  TRANSFORM_SIGNATURE(FILE *, BM_merge_pair,                             \
                      const struct BM_filename_pair *const pair)         \
  TRANSFORM_SIGNATURE(BM_blocks, BM_merge_pairs, const BM_pairs pairs)   \
  TRANSFORM_SIGNATURE(void, BM_delete_block, BM_block *block)            \
  TRANSFORM_SIGNATURE(void, BM_delete_row, BM_row *row)                  \
  TRANSFORM_SIGNATURE(size_t, BM_get_rows_count, const BM_blocks blocks, \
                      const size_t index)                                \
  TRANSFORM_SIGNATURE(void, BM_print_blocks, const BM_blocks blocks)     \
  TRANSFORM_SIGNATURE(void, BM_free_blocks, BM_blocks blocks)            \
  TRANSFORM_SIGNATURE(void, BM_free_block, BM_block block)               \
  TRANSFORM_SIGNATURE(void, BM_free_row, BM_row row)                     \
  TRANSFORM_SIGNATURE(void, BM_free_pairs, BM_pairs pair)

// global handles for functions passed as args, const ptr for shared/static,
// plain for dynamic - will be reassigned in main
#ifdef DYNAMIC
#define FPTR_DECL(ret, f_name, ...) ret (*fptr_##f_name)(__VA_ARGS__);
#else
#define FPTR_DECL(ret, f_name, ...) \
  ret (*const fptr_##f_name)(__VA_ARGS__) = f_name;
#endif

LIST_BM_FUNCTIONS(FPTR_DECL);

#undef DEF_FPTR

bool try_extracting_file_pair(char **next_token, char **rest);

int main(int argc, char *argv[]) {
#ifdef DYNAMIC
  void *dl_handle = dlopen("libblock_manager.so", RTLD_LAZY);
  assert(dl_handle != NULL);
#define FPTR_INIT(ret, name, ...) fptr_##name = dlsym(dl_handle, #name);
  LIST_BM_FUNCTIONS(FPTR_INIT)
#undef FPTR_INIT
#endif

  BM_pairs filename_pairs = NULL;
  BM_blocks blocks = NULL;

  for (int i = 1; i < argc; ++i) {
    const char *extracted = argv[i];
    if (!strcmp(extracted, "merge_files")) {
      char *next_token = NULL;
      char **current_s = &argv[++i];
      // MEASURE(
      // "parse & allocate",
      while (try_extracting_file_pair(&next_token, current_s)) {
        // *move* ptrs to pairs
        fptr_BM_add_pair(&filename_pairs, next_token, *current_s);
        current_s = &argv[++i];
      }
      blocks = fptr_BM_merge_pairs(filename_pairs);
      // )
      fptr_BM_free_pairs(filename_pairs);
      argv[i] = next_token;
      --i;
    } else if (!strcmp(extracted, "remove_block")) {
      const unsigned int index = strtol(argv[++i], NULL, 10);
      fptr_BM_delete_block(&blocks[index]);
    } else if (!strcmp(extracted, "remove_row")) {
      const unsigned int block_index = strtol(argv[++i], NULL, 10);
      const unsigned int row_index = strtol(argv[++i], NULL, 10);
      fptr_BM_delete_row(&blocks[block_index][row_index]);
    } else {
      printf("unrecognized: %s\n", extracted);
    }

    fptr_BM_free_blocks(blocks);
  }

#ifdef DYNAMIC
  dlclose(dl_handle);
#endif
  return 0;
}

bool try_extracting_file_pair(char **next_token, char **rest) {
  *next_token = strsep(rest, ":");
  return *rest != NULL;
}