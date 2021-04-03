#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/times.h>

#include "lib/block_manager.h"
#include "lib/vector.h"

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
    printf(MSG ": real, user, system\n");                                \
    printf("%ld %ld %ld\n", real_elapsed, user_elapsed, system_elapsed); \
  } while (0);

bool try_extracting_file_pair(char** next_token, char** rest);

int main(int argc, char* argv[]) {
  BM_pairs filename_pairs = NULL;
  BM_blocks blocks = NULL;
  int i = 0;
  char* next_token = NULL;
  char** current_s = &argv[++i];
  // MEASURE(
  //     "parse & allocate",
  while (try_extracting_file_pair(&next_token, current_s)) {
    // *move* ptrs to pairs
    BM_add_pair(&filename_pairs, next_token, *current_s);
    current_s = &argv[++i];
  }

  printf("%ld\n", vec_get_size(filename_pairs));

  BM_merge_pairs(&blocks, filename_pairs);
  // )
  BM_free_pairs(filename_pairs);
  argv[i] = next_token;
  --i;

  BM_free_blocks(blocks);

  return 0;
}

bool try_extracting_file_pair(char** next_token, char** rest) {
  *next_token = strsep(rest, ":");
  return *rest != NULL;
}