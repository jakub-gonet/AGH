#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SIZE 31

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

int main(void) {
  int z;
  scanf("%d", &z);

  while (z--) {
    int n, k;

    char buf_surname[MAX_SIZE];
    char buf_tel_no[MAX_SIZE];

    scanf("%d %d", &n, &k);
    int m = 2;
    while (m < n) {
      m *= 2;
    }
    hashmap* map = HM_init_hashmap(m);
    while (k--) {
      char operation_type;
      scanf(" %c %s", &operation_type, buf_surname);
      switch (operation_type) {
        case 'a':;
          scanf(" %s", buf_tel_no);
          HM_add(map, buf_surname, buf_tel_no);
          break;
        case 'r':;
          HM_remove(map, buf_surname);
          break;
        case 'g':;
          int index = HM_find(map, buf_surname);
          if (index != -1) {
            char* tel_no = map->data[index]->tel_no;
            printf("%s", tel_no);
          }
          printf("\n");
          break;
      }
    }
    HM_free_hashmap(map);
  }
}
