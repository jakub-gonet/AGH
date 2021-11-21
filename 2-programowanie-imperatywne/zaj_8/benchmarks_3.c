#define _XOPEN_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

char* copy_str(char* str) {
  int len = strlen(str);
  char* copy = malloc((len + 1) * sizeof(char));
  strcpy(copy, str);
  return copy;
}

typedef enum state { PRESENT, NIL, DELETED } state;
typedef struct entry {
  char* surname;
  char* tel_no;
  state s;
} entry;

entry* HM_init_entry(char* surname, char* tel_no, state s) {
  entry* e = malloc(sizeof(entry));
  e->surname = surname;
  e->tel_no = tel_no;
  e->s = s;
  return e;
}

void HM_free_entry(entry* e) {
  if (e->surname != NULL) {
    free(e->surname);
  }
  if (e->tel_no != NULL) {
    free(e->tel_no);
  }
  free(e);
}

typedef struct hashmap {
  entry** data;
  int size;
} hashmap;

hashmap* HM_init_hashmap(int n) {
  hashmap* m = malloc(sizeof(hashmap));
  m->size = n;
  m->data = malloc(n * sizeof(entry*));
  for (int i = 0; i < n; i++) {
    m->data[i] = HM_init_entry(NULL, NULL, NIL);
  }

  return m;
}

void HM_free_hashmap(hashmap* m) {
  for (int i = 0; i < m->size; i++) {
    HM_free_entry(m->data[i]);
  }
  free(m->data);
  free(m);
}

int HM_hash(char* str, int m) {
  const int p = 19;
  int hash_v = 0;
  int p_pow = 1;
  for (int i = 0; str[i] != '\0'; i++) {
    hash_v = (hash_v + str[i] * p_pow) % m;
    p_pow = (p_pow * p) % m;
  }
  return hash_v;
}

int HM_hash_2(char* str, int m) {
  const int p = 43;
  int hash_v = 0;
  int p_pow = 1;
  for (int i = 0; str[i] != '\0'; i++) {
    hash_v = (hash_v + str[i] * p_pow) % m;
    p_pow = (p_pow * p) % m;
  }
  return hash_v + ((hash_v + 1) % 2);
}

int collisions = 0;
int HM_add(hashmap* m, char* surname, char* tel_no) {
  int h = HM_hash(surname, m->size);
  int h2 = HM_hash_2(surname, m->size);
  for (int i = 0; i < m->size; i++) {
    int index = (h + i * h2) % m->size;

    state s = m->data[index]->s;

    if (s == NIL || s == DELETED) {
      m->data[index]->surname = copy_str(surname);
      m->data[index]->tel_no = copy_str(tel_no);
      m->data[index]->s = PRESENT;

      return index;
    }
    collisions++;
  }
  return -1;
}

int HM_find(hashmap* m, char* surname) {
  int h = HM_hash(surname, m->size);
  int h2 = HM_hash_2(surname, m->size);
  for (int i = 0; i < m->size; i++) {
    int index = (h + i * h2) % m->size;

    state s = m->data[index]->s;
    if (s == NIL) {
      return -1;
    }
    if (s == DELETED) {
      continue;
    }

    if (!strcmp(surname, m->data[index]->surname)) {
      return index;
    }
  }
  return -1;
}

void HM_remove(hashmap* m, char* surname) {
  int i = HM_find(m, surname);
  free(m->data[i]->surname);
  m->data[i]->surname = NULL;
  free(m->data[i]->tel_no);
  m->data[i]->tel_no = NULL;
  m->data[i]->s = DELETED;
}

void rand_str(char* dest, size_t length) {
  char charset[] =
      "0123456789"
      "abcdefghijklmnopqrstuvwxyz"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  while (length-- > 0) {
    size_t index = lrand48() / RAND_MAX * (sizeof charset - 1);
    *dest++ = charset[index];
  }
  *dest = '\0';
}

#define MAX_SIZE 10
#define MAX_SIZE_WIDTH 100000000
int main(void) {
  const int size_n = 10;
  int n_test[] = {10,    100,   500,    1000,    10000,
                  30000, 50000, 100000, 1000000, 2000000};
  printf("Add / remove test quadratic:\n");
  for (int size = 0; size < size_n; size++) {
    int m = 2;
    while (m < n_test[size]) {
      m *= 2;
    }

    hashmap* map = HM_init_hashmap(m);
    char buf_surname[MAX_SIZE + 1];
    char buf_tel_no[MAX_SIZE + 1];

    // // CLOCK
    clock_t t0, t1;
    t0 = clock();
    while (t0 == (t1 = clock()))
      ;
    t0 = t1;
    // ADD
    for (int i = 0; i < m; i++) {
      sprintf(buf_surname, "%d", i);
      rand_str(buf_tel_no, MAX_SIZE);

      HM_add(map, buf_surname, buf_tel_no);
    }

    for (int i = m - 1; i >= 0; i--) {
      sprintf(buf_surname, "%d", i);
      HM_remove(map, buf_surname);
    }

    // // CLOCK
    t1 = clock();

    double time_mea = (t1 - t0) * (1.0 / CLOCKS_PER_SEC);
    printf("%f\n", time_mea);
    // printf("%d\n", collisions);
    HM_free_hashmap(map);
  }
}
