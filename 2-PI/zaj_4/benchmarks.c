#define _XOPEN_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef void (*gen)(int*, int);
typedef int (*pivot_f)(int* arr, const int start_i, const int end_i);

inline void swap(int* a, int* b) {
  const int tmp = *a;
  *a = *b;
  *b = tmp;
}
void generate_random_mod_10(int* arr, int n) {
  //	You can experiment with the modulo value.
  //	The bigger number the less duplicates in the array.
  for (int i = 0; i < n; ++i) arr[i] = lrand48() % 10;
}

void generate_random_mod_100(int* arr, int n) {
  //	You can experiment with the modulo value.
  //	The bigger number the less duplicates in the array.
  for (int i = 0; i < n; ++i) arr[i] = lrand48() % 100;
}

void generate_random_mod_1000(int* arr, int n) {
  //	You can experiment with the modulo value.
  //	The bigger number the less duplicates in the array.
  for (int i = 0; i < n; ++i) arr[i] = lrand48() % 1000;
}

void generate_random_mod_1000000(int* arr, int n) {
  //	You can experiment with the modulo value.
  //	The bigger number the less duplicates in the array.
  for (int i = 0; i < n; ++i) arr[i] = lrand48() % 1000000;
}

void generate_same_small(int* arr, int n) {
  for (int i = 0; i < n; ++i) arr[i] = 16;
}

void generate_same_big(int* arr, int n) {
  for (int i = 0; i < n; ++i) arr[i] = 32767;
}

void generate_sorted(int* arr, int n) {
  for (int i = 0; i < n; ++i) arr[i] = i;
}

void generate_sorted_reverse(int* arr, int n) {
  for (int i = 0; i < n; ++i) arr[i] = n - i;
}

void generate_almost_sorted_10(int* arr, int n) {
  for (int i = 0; i < n; ++i) arr[i] = i;
  for (int i = 0; i < 5; ++i) {
    int j = lrand48() % n;
    int k = lrand48() % n;
    while (j == k) k = lrand48() % n;
    swap(&arr[j], &arr[k]);
  }
}

void generate_almost_sorted_1_percent(int* arr, int n) {
  for (int i = 0; i < n; ++i) arr[i] = i;
  for (int i = 0; i < n / 200; ++i) {
    int j = lrand48() % n;
    int k = lrand48() % n;
    while (j == k) k = lrand48() % n;
    swap(&arr[j], &arr[k]);
  }
}

void generate_almost_sorted_1_permill(int* arr, int n) {
  for (int i = 0; i < n; ++i) arr[i] = i;
  for (int i = 0; i < n / 2000; ++i) {
    int j = lrand48() % n;
    int k = lrand48() % n;
    while (j == k) k = lrand48() % n;
    swap(&arr[j], &arr[k]);
  }
}

void generate_almost_sorted_reverse_10(int* arr, int n) {
  for (int i = 0; i < n; ++i) arr[i] = n - i;
  for (int i = 0; i < 5; ++i) {
    int j = lrand48() % n;
    int k = lrand48() % n;
    while (j == k) k = lrand48() % n;
    swap(&arr[j], &arr[k]);
  }
}

void generate_almost_sorted_reverse_1_percent(int* arr, int n) {
  for (int i = 0; i < n; ++i) arr[i] = n - i;
  for (int i = 0; i < n / 200; ++i) {
    int j = lrand48() % n;
    int k = lrand48() % n;
    while (j == k) k = lrand48() % n;
    swap(&arr[j], &arr[k]);
  }
}

void generate_almost_sorted_reverse_1_permill(int* arr, int n) {
  for (int i = 0; i < n; ++i) arr[i] = n - i;
  for (int i = 0; i < n / 2000; ++i) {
    int j = lrand48() % n;
    int k = lrand48() % n;
    while (j == k) k = lrand48() % n;
    swap(&arr[j], &arr[k]);
  }
}

// -------- QSORT
int partition(int* const arr, const int start_i, const int end_i, pivot_f f) {
  // keeping pivot in fixed position at end of hunk
  const int pivot_i = (*f)(arr, start_i, end_i);
  swap(&arr[pivot_i], &arr[end_i]);

  const int pivot = arr[end_i];

  int to_swap_i = start_i;
  for (int i = start_i; i <= end_i; ++i) {
    // we keep index of element which will be swapped if i-th element is less or
    // eq than pivot, which means that to_swap_i is index of number potentially
    // bigger than pivot. Elements left to to_swap_i are surely smaller than
    // pivot
    if (arr[i] < pivot) {
      swap(&arr[to_swap_i], &arr[i]);
      ++to_swap_i;
    }
  }

  swap(&arr[to_swap_i], &arr[end_i]);

  return to_swap_i;
}

void _quicksort(int* const arr, const int start_i, const int end_i, pivot_f f) {
  if (start_i < end_i) {
    const int pivot_i = partition(arr, start_i, end_i, f);
    _quicksort(arr, start_i, pivot_i - 1, f);
    _quicksort(arr, pivot_i + 1, end_i, f);
  }
}
inline void quicksort(int* const arr, const int n, pivot_f f) {
  _quicksort(arr, 0, n - 1, f);
}
// --------

void printArray(int* arr, int n) {
  for (int i = 0; i < n; ++i) printf("%d ", arr[i]);
  printf("\n");
}

double timetest(int* arr, int n, gen fgen, pivot_f f) {
  clock_t t0, t1;
  fgen(arr, n);

  //	Wait for clock() roll over before starting
  t0 = clock();
  while (t0 == (t1 = clock()))
    ;

  t0 = t1;
  quicksort(arr, n, f);
  t1 = clock();

  return (t1 - t0) * (1.0 / CLOCKS_PER_SEC);
}

double test_mean(int test_n, int* arr, int n, gen fgen, pivot_f f) {
  double times = 0;
  for (size_t i = 0; i < test_n; ++i) {
    times += timetest(arr, n, fgen, f);
  }
  return times / test_n;
}

int pivot_at_start(int* arr, int start_i, int end_i) { return start_i; }
int pivot_at_end(int* arr, int start_i, int end_i) { return end_i; }
int pivot_at_middle(int* arr, int start_i, int end_i) {
  return (start_i + end_i) / 2;
}
int randomized_pivot(int* arr, int start_i, int end_i) {
  int mod = (end_i - start_i + 1);
  int r;
  int limit = RAND_MAX - (RAND_MAX % mod);
  while ((r = rand()) >= limit)
    ;
  return start_i + (r % mod);
}
int pivot_median_of_three(int* arr, int start_i, int end_i) {
  int mid = (start_i + end_i) / 2;
  if (arr[end_i] < arr[start_i]) {
    swap(&arr[start_i], &arr[end_i]);
  }
  if (arr[mid] < arr[start_i]) {
    swap(&arr[mid], &arr[start_i]);
  }
  if (arr[end_i] < arr[mid]) {
    swap(&arr[end_i], &arr[mid]);
  }
  return mid;
}

typedef struct pivot_s {
  char* name;
  pivot_f f;
} pivot_s;

typedef struct generated_data_s {
  gen f;
  char* name;
} generated_data_s;
#define PIVOT_F_SIZE 5
#define GEN_F_SIZE 14
#define N_SIZE 5

pivot_s pivots_f[PIVOT_F_SIZE] = {
    {.f = pivot_at_start, .name = "pivot_at_start"},
    {.f = pivot_at_end, .name = "pivot_at_end"},
    {.f = pivot_at_middle, .name = "pivot_at_middle"},
    {.f = randomized_pivot, .name = "randomized_pivot"},
    {.f = pivot_median_of_three, .name = "pivot_median_of_three"}};

generated_data_s gen_functions[GEN_F_SIZE] = {
    {.f = generate_same_small, .name = "generate_same_small"},
    {.f = generate_same_big, .name = "generate_same_big"},
    {.f = generate_random_mod_10, .name = "generate_random_mod_10"},
    {.f = generate_random_mod_100, .name = "generate_random_mod_100"},
    {.f = generate_random_mod_1000, .name = "generate_random_mod_1000"},
    {.f = generate_random_mod_1000000, .name = "generate_random_mod_1000000"},
    {.f = generate_sorted, .name = "generate_sorted"},
    {.f = generate_almost_sorted_10, .name = "generate_almost_sorted_10"},
    {.f = generate_almost_sorted_1_percent,
     .name = "generate_almost_sorted_1_percent"},
    {.f = generate_almost_sorted_1_permill,
     .name = "generate_almost_sorted_1_permill"},
    {.f = generate_sorted_reverse, .name = "generate_sorted_reverse"},
    {.f = generate_almost_sorted_reverse_10,
     .name = "generate_almost_sorted_reverse_10"},
    {.f = generate_almost_sorted_reverse_1_percent,
     .name = "generate_almost_sorted_reverse_1_percent"},
    {.f = generate_almost_sorted_reverse_1_permill,
     .name = "generate_almost_sorted_reverse_1_permill"}};

int n_sizes[N_SIZE] = {3, 10, 100, 1000, 100000};
void print_headers(void) {
  // headers
  for (size_t i = 0; i < GEN_F_SIZE; i++) {
    const generated_data_s gen_s = gen_functions[i];
    printf("\t%s", gen_s.name);
  }
  printf("\n");
}

void print_results(int sample_size, int* arr, int n, pivot_f piv_f) {
  for (size_t k = 0; k < GEN_F_SIZE; k++) {
    const generated_data_s gen_s = gen_functions[k];
    const double elapsed_time_ms =
        test_mean(sample_size, arr, n, gen_s.f, piv_f) * 1000;
    printf("\t%.4f", elapsed_time_ms);
  }
  printf("\n");
}

void print_results_smaller(gen gen_f, int sample_size, int* arr, int n,
                           pivot_f piv_f) {
  const double elapsed_time_ms =
      test_mean(sample_size, arr, n, gen_f, piv_f) * 1000;
  printf("%d\t%.4f", n, elapsed_time_ms);
}

int* alloc(int n) {
  int* arr = malloc(n * sizeof(int));
  if (arr == NULL) {
    printf("allocation failed\n");
    exit(1);
  }
  return arr;
}

void base_tests(int sample_size) {
  print_headers();

  for (size_t i = 0; i < PIVOT_F_SIZE; i++) {
    const pivot_s piv_s = pivots_f[i];
    printf("Using pivot f: %s\n", piv_s.name);

    for (size_t j = 0; j < N_SIZE; j++) {
      const int n = n_sizes[j];
      int* arr = alloc(n);

      print_results(sample_size, arr, n, piv_s.f);

      free(arr);
    }
  }
}

void continuos_tests(int sample_size, gen f) {
  for (size_t i = 0; i < PIVOT_F_SIZE; i++) {
    const pivot_s piv_s = pivots_f[i];
    printf("\t%s", piv_s.name);
  }
  printf("\n");

  for (size_t n = 3; n < 30000; n += 1000) {
    int* arr = alloc(n);
    for (size_t i = 0; i < PIVOT_F_SIZE; i++) {
      const pivot_s piv_s = pivots_f[i];
      print_results_smaller(f, sample_size, arr, n, piv_s.f);
    }
    free(arr);
    printf("\n");
  }
}

int main() {
  srand48(time(0));
  const int sample_size = 5;
  // base_tests(sample_size);
  // continuos_tests(sample_size, generate_sorted);
  // continuos_tests(sample_size, generate_random_mod_1000000);
  // continuos_tests(sample_size, generate_sorted_reverse);
  return 0;
}
