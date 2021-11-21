#define _XOPEN_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef struct data {
  char* surname;
  char* tel_no;
} data;

typedef struct node {
  struct node* prev;
  struct node* next;
  data data;
} node;

char* copy_str(char* str) {
  int len = strlen(str);
  char* copy = malloc((len + 1) * sizeof(char));
  strcpy(copy, str);
  return copy;
}

node* init_node(data* d) {
  node* n = malloc(sizeof(node));
  n->prev = NULL;
  n->next = NULL;
  n->data.surname = copy_str(d->surname);
  n->data.tel_no = copy_str(d->tel_no);
  return n;
}

void free_data(data* d) {
  free(d->surname);
  free(d->tel_no);
}

void free_node(node* n) {
  free_data(&n->data);
  free(n);
}

void free_linked_list(node* head) {
  while (head != NULL) {
    node* n = head;
    head = head->next;
    free_node(n);
  }
}

typedef struct hashmap {
  node** data;
  int size;
} hashmap;

hashmap init_hashmap(int n) {
  hashmap m;
  m.size = n;
  m.data = malloc(n * sizeof(*m.data));
  for (int i = 0; i < n; i++) {
    m.data[i] = NULL;
  }

  return m;
}

void free_hashmap(hashmap* m) {
  for (int i = 0; i < m->size; i++) {
    free_linked_list(m->data[i]);
  }
  free(m->data);
}

int hash(char* str, int m) {
  const int p = 19;
  int hash_v = 0;
  int p_pow = 1;
  for (int i = 0; str[i] != '\0'; i++) {
    hash_v = (hash_v + str[i] * p_pow) % m;
    p_pow = (p_pow * p) % m;
  }
  return hash_v;
}

void HM_add(hashmap* m, data* d) {
  int h = hash(d->surname, m->size);
  node* head = m->data[h];

  node* new = init_node(d);

  if (head != NULL) {
    new->next = head;
    head->prev = new;
  }
  m->data[h] = new;
}

void HM_remove(hashmap* m, char* surname) {
  int h = hash(surname, m->size);
  node* head = m->data[h];

  while (head != NULL) {
    if (!strcmp(head->data.surname, surname)) {
      // not first node
      if (head->prev != NULL) {
        head->prev->next = head->next;
      } else {
        m->data[h] = head->next;
      }
      // not last node
      if (head->next != NULL) {
        head->next->prev = head->prev;
      } else {
        if (head->prev != NULL)
          head->prev->next = NULL;
      }

      free_node(head);
      break;
    }
    head = head->next;
  }
}

char* HM_find(hashmap* m, char* surname) {
  int h = hash(surname, m->size);
  node* head = m->data[h];
  while (head != NULL) {
    if (!strcmp(head->data.surname, surname)) {
      return head->data.tel_no;
    }
    head = head->next;
  }
  return NULL;
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
#define MAX_SIZE_WIDTH 1000000000
int main(void) {
  const int size_n = 2;
  int n_test[] = {10,    100,   500,    1000,    10000,
                  30000, 50000, 100000, 1000000, 10000000};
  printf("Add / remove test:\n");
  for (int size = 0; size < size_n; size++) {
    // Linked lists
    hashmap map = init_hashmap(n_test[size]);
    char buf_surname[MAX_SIZE + 1];
    char buf_tel_no[MAX_SIZE + 1];
    data d;
    d.surname = buf_surname;
    d.tel_no = buf_tel_no;

    // // CLOCK
    // clock_t t0, t1;
    // t0 = clock();
    // while (t0 == (t1 = clock()))
    //   ;
    // t0 = t1;
    // ADD
    for (int i = 0; i < n_test[size]; i++) {
      sprintf(buf_surname, "%d", i % MAX_SIZE_WIDTH);
      rand_str(buf_tel_no, MAX_SIZE);

      HM_add(&map, &d);
    }

    // for (int i = n_test[size]; i >= 0; i--) {
    //   sprintf(buf_surname, "%d", i % MAX_SIZE_WIDTH);

    //   HM_remove(&map, buf_surname);
    // }

    // CLOCK
    // t1 = clock();

    // double time_mea = (t1 - t0) * (1.0 / CLOCKS_PER_SEC);
    // printf("%f\n", n_test[size], time_mea);
    int clusters = 0;
    for (int i = 0; i < map.size; i++) {
      if (map.data[i] != NULL) {
        clusters++;
      }
    }
    printf("clusters: %d\n", clusters);
    free_hashmap(&map);
  }
}