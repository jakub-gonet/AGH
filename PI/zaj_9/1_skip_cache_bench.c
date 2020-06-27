#define _XOPEN_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct node {
  int value;

  struct node* next[];
} node;

int max_height = -1;

node* create_node(int v) {
  node* new_node =
      malloc(sizeof(*new_node) + sizeof(*new_node->next) * max_height);
  new_node->value = v;
  for (int i = 0; i < max_height; ++i) {
    new_node->next[i] = NULL;
  }
  return new_node;
}

node* alloc_skiplist() {
  return create_node(-1);
}

void dealloc_skiplist(node* head) {
  while (head != NULL) {
    node* to_free = head;
    head = head->next[0];
    free(to_free);
  }
}

int gen_list_height() {
  int h = 1;
  while ((rand() % 2) && h < max_height) {
    ++h;
  }

  return h;
}

node* find_closest_smaller_prev(node* head, int level, int v) {
  while (head->next[level] != NULL && head->next[level]->value < v) {
    head = head->next[level];
  }
  return head;
}

void skiplist_append(node* head, int v) {
  node* new_node = create_node(v);
  int height = gen_list_height();
  int level = max_height;
  node* list = head;
  while (level--) {
    list = find_closest_smaller_prev(list, level, v);
    if (level <= height) {
      new_node->next[level] = list->next[level];
      list->next[level] = new_node;
    }
  };
}

void skiplist_remove(node* head, int v) {
  int level = max_height;
  node* list = head;
  node* to_remove = NULL;

  while (level--) {
    list = find_closest_smaller_prev(list, level, v);
    node* next = list->next[level];
    if (next != NULL && next->value == v) {
      to_remove = next;
      list->next[level] = to_remove->next[level];
    }
  }
  if (to_remove != NULL) {
    free(to_remove);
  }
}

int skiplist_includes(node* head, int v) {
  int level = max_height;
  node* list = head;

  while (level--) {
    list = find_closest_smaller_prev(list, level, v);
    node* next = list->next[level];
    if (next != NULL && next->value == v) {
      return 1;
    }
  }
  return 0;
}

void debug(node* head) {
  for (int i = max_height - 1; i >= 0; i--) {
    printf("%d:\t", i);
    node* node = head;
    while (node != NULL) {
      printf("%d -> ", node->value);
      node = node->next[i];
    }
    printf("null\n");
  }
}

#define SIZE(x) (sizeof(x) / sizeof((x)[0]))
int main(void) {
  srand(2137);

  int n_test[] = {10, 100, 1000, 3000, 5000, 10000, 30000, 50000};
  int heights[] = {3, 5, 10, 100};

  for (int j = 0; j < SIZE(heights); j++) {
    // printf("height: %d\n", heights[j]);
    for (int i = 0; i < SIZE(n_test); i++) {
      //   printf("APPEND\n");
      max_height = heights[j];

      node* list = alloc_skiplist();

      // CLOCK

      clock_t t0, t1;
      t0 = clock();
      while (t0 == (t1 = clock()))
        ;
      t0 = t1;

      // CODE
      // APPEND
      for (int k = 0; k < n_test[i]; k++) {
        skiplist_append(list, k);
      }

      // CLOCK
      t1 = clock();
      double time_mea = (t1 - t0) * (1.0 / CLOCKS_PER_SEC);
      printf("%f\t", time_mea);

      // REMOVE
      //   printf("REMOVE\n");
      t0 = clock();
      while (t0 == (t1 = clock()))
        ;
      t0 = t1;
      for (int k = 0; k < n_test[i]; k++) {
        skiplist_remove(list, k);
      }

      // CLOCK
      t1 = clock();
      time_mea = (t1 - t0) * (1.0 / CLOCKS_PER_SEC);
      printf("%f\n", time_mea);

      dealloc_skiplist(list);
    }
  }
}
