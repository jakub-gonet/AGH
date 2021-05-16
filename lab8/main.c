#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PATH "result.pgm"

enum mode_t { COLUMN, MOD };

struct iter_thread_config_t {
  size_t col_i;
  size_t row_i;
};

struct iter_config_t {
  size_t threads_n;
  size_t width;
  size_t height;
  struct iter_thread_config_t* d;
};

typedef bool (*iter_config_f)(struct iter_config_t*, size_t);

size_t ceil_i(size_t x, size_t y) {
  return x / y + (x % y != 0);
}

bool column_iter(struct iter_config_t* config, size_t thread_i) {
  assert(config->threads_n <= config->width);
  size_t end = ceil_i((thread_i + 1) * config->width, config->threads_n);
  size_t old_row_i = config->d[thread_i].row_i;
  config->d[thread_i].row_i = (config->d[thread_i].row_i + 1) % config->height;
  config->d[thread_i].col_i += config->d[thread_i].row_i != old_row_i + 1;
  return config->d[thread_i].col_i < end;
}

bool mod_iter(struct iter_config_t* config, size_t thread_i) {
  int shifted = config->d[thread_i].col_i + config->threads_n;
  config->d[thread_i].col_i = shifted % config->width;
  config->d[thread_i].row_i += shifted / config->width;

  return config->d[thread_i].row_i < config->height &&
         config->d[thread_i].col_i < config->width;
}

size_t to_size_t(const char* str) {
  errno = 0;
  size_t val = (size_t)strtol(str, NULL, 10);
  assert(errno == 0);
  return val;
}

struct iter_config_t init_thread_config(size_t threads_n,
                                        size_t width,
                                        size_t height,
                                        enum mode_t mode) {
  struct iter_thread_config_t* config =
      malloc(sizeof(struct iter_thread_config_t) * threads_n);

  for (size_t i = 0; i < threads_n; ++i) {
    if (mode == MOD) {
      config[i].col_i = i % width;
      config[i].row_i = i / width;
    } else if (mode == COLUMN) {
      config[i].row_i = 0;
      config[i].col_i = ceil_i(i * width, threads_n);
    }
  }

  return (struct iter_config_t){
      .d = config, .height = height, .width = width, .threads_n = threads_n};
}

void free_thread_config(struct iter_config_t* config) {
  free(config->d);
}

void free_image(unsigned char** image, size_t height) {
  for (size_t i = 0; i < height; i++) {
    free(image[i]);
  }
  free(image);
}

enum mode_t get_mode(const char* mode) {
  if (strcmp(mode, "col") == 0) {
    return COLUMN;
  } else if (strcmp(mode, "mod") == 0) {
    return MOD;
  } else {
    return -1;
  }
}

void invert_image(unsigned char** image,
                  const iter_config_f iter_f,
                  struct iter_config_t* threads_config,
                  const size_t thread_i) {
  printf("[%ld] (%ld, %ld)\n", thread_i, threads_config->d[thread_i].row_i,
         threads_config->d[thread_i].col_i);
  do {
    const struct iter_thread_config_t config = threads_config->d[thread_i];
    // printf("Processing (%d, %d): %d -> %d\n", config.row_i, config.col_i,
    //        image[config.row_i][config.col_i],
    //        255 - image[config.row_i][config.col_i]);
    // if (!(config.row_i < threads_config->height &&
    //       config.col_i < threads_config->width)) {
    //   printf("[%ld] (%ld, %ld)\n", thread_i, config.row_i, config.col_i);
    //   exit(1);
    // }
    image[config.row_i][config.col_i] = 255 - image[config.row_i][config.col_i];
  } while (iter_f(threads_config, thread_i));
}

void load_pgm_header(FILE* image, size_t* width, size_t* height) {
  char* line = NULL;
  size_t len = 0;
  ssize_t read;
  // P2
  read = getline(&line, &len, image);
  assert(read != -1);
  assert(strcmp(line, "P2\n") == 0);

  // W H
  read = getline(&line, &len, image);
  assert(read != -1);
  sscanf(line, "%ld %ld", width, height);
  // max color
  read = getline(&line, &len, image);
  assert(read != -1);

  // color depth
  size_t color;
  sscanf(line, "%ld", &color);
  assert(color == 255);
  free(line);
}

unsigned char** load_pgm_image(const char* input,
                               size_t* width,
                               size_t* height) {
  FILE* file = fopen(input, "r");
  assert(file != NULL);

  char* line = NULL;
  size_t len = 0;
  ssize_t read;

  load_pgm_header(file, width, height);

  unsigned char** image = malloc(sizeof(*image) * *height);
  for (size_t i = 0; i < *height; i++) {
    image[i] = malloc(sizeof(**image) * *width);
  }
  assert(image != NULL);
  for (size_t row = 0; (read = getline(&line, &len, file)) != -1; ++row) {
    assert(row < *height);
    int pos = 0;
    for (size_t col = 0; col < *width; ++col) {
      assert(pos < read);
      int r;
      sscanf(&line[pos], "%hhu%n", &image[row][col], &r);
      pos += r;
    }
  }

  fclose(file);
  if (line) {
    free(line);
  }
  return image;
}

void save_pgm_image(const char* path,
                    unsigned char** image,
                    size_t width,
                    size_t height) {
  FILE* file = fopen(path, "w");
  assert(file != NULL);
  fprintf(file, "P2\n%ld %ld\n255\n", width, height);
  for (size_t row = 0; row < height; ++row) {
    for (size_t col = 0; col < width; ++col) {
      fprintf(file, "%hhu ", image[row][col]);
    }
    fprintf(file, "\n");
  }
  fclose(file);
}

int main(int argc, char const* argv[]) {
  assert(argc == 3 + 1);

  const char* input = argv[1];
  const size_t threads_n = to_size_t(argv[2]);
  enum mode_t mode = get_mode(argv[3]);

  iter_config_f config_f;
  if (mode == COLUMN) {
    config_f = &column_iter;
  } else if (mode == MOD) {
    config_f = &mod_iter;
  } else {
    exit(EXIT_FAILURE);
  }

  size_t width, height;
  unsigned char** image = load_pgm_image(input, &width, &height);

  // for (size_t row = 0; row < height; ++row) {
  //   for (size_t col = 0; col < width; col++) {
  //     printf(image[row][col] == 0 ? "%3hhu " : "%4hhu", image[row][col]);
  //   }
  //   printf("\n");
  // }

  struct iter_config_t threads_config =
      init_thread_config(threads_n, width, height, mode);

  for (size_t i = 0; i < threads_n; ++i) {
    invert_image(image, config_f, &threads_config, i);
  }
  // printf("---\n");

  // for (size_t row = 0; row < height; ++row) {
  //   for (size_t col = 0; col < width; col++) {
  //     printf(image[row][col] == 0 ? "%3hhu " : "%4hhu",
  //            (unsigned)image[row][col]);
  //   }
  //   printf("\n");
  // }

  free_thread_config(&threads_config);
  save_pgm_image(PATH, image, width, height);
  free_image(image, height);
  return 0;
}
