#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char const *argv[]) {
  for (size_t i = 1; i < argc; ++i) {
    const char *extracted = argv[i];
    if (!strcmp(extracted, "create_table")) {
      const unsigned int size = strtol(argv[++i], NULL, 10);
      printf("create_table %d\n", size);
    } else if (!strcmp(extracted, "merge_files")) {
      printf("create_table\n");
    } else if (!strcmp(extracted, "remove_block")) {
      const unsigned int index = strtol(argv[++i], NULL, 10);
      printf("remove_block\n");
    } else if (!strcmp(extracted, "remove_row")) {
      const unsigned int block_index = strtol(argv[++i], NULL, 10);
      const unsigned int row_index = strtol(argv[++i], NULL, 10);
      printf("remove_row %d %d\n", block_index, row_index);
    } else {
      printf("unrecognized\n");
    }
  }

  return 0;
}
