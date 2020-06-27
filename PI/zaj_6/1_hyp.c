/*
1. Hyperloop

1 Zadanie

Dana jest mapa połączeń między miastami. Napisz program, który wczyta mapę
połączeń, a następnie stwierdzi, czy między podanymi parami miast istnieją
połączenia.

2 Wejście

W pierwszym wierszu standardowego wejścia znajdują się trzy liczby: n to liczba
miast (wierzchołków grafu), k to liczba istniejących połączeń między miastami, a
p to liczba połączeń wyszukiwanych przez użytkowników systemu. Następne k linii
zawiera pary liczb całkowitych z przedziału [0, n−1] - istniejące połączenia
między poszczególnymi miastami (krawędzie grafu). Kolejne p linii zawiera pary
liczb całkowitych z przedziału [0, n − 1] - połączenia wyszukiwane przez
użytkowników systemu.

3 Wyjście

Na standardowym wyjściu programu powinno znaleźć się p linii, każda zawierająca
jedno słowo - YES, jeśli istnieje połączenie między podanymi miastami, NO w
przeciwnym przypadku.

4 Przykład

4.1 Wejście

5 3 3
0 1
1 2
3 4
0 2
4 3
1 4

4.2 Wyjście

YES
YES
NO
*/

#include <stdio.h>
#include <stdlib.h>

/* QUEUE */

typedef struct queue {
  int *d;
  int back;
  int front;
  int size;
} queue;

queue alloc_queue(int max_size) {
  return (queue){.size = max_size,
                 .front = 0,
                 .back = 0,
                 .d = malloc(max_size * sizeof(int))};
}
void dealloc_queue(queue q) { free(q.d); }

void put(queue *q, int v) { q->d[q->back++ % q->size] = v; }

int get(queue *q) { return q->d[q->front++ % q->size]; }

int is_empty(queue *q) { return q->front == q->back; }

/* ARRAY */

typedef struct array {
  int *d;
  int size;
} array;

array alloc_array(int max_size) {
  return (array){.size = 0, .d = calloc(max_size, sizeof(int))};
}
void append(array *a, int v) { a->d[a->size++] = v; }

void dealloc_array(array a) { free(a.d); }

/* ADJ LIST */

array *alloc_adj_list(int vert_n, int max_neighbour_n) {
  array *adj_list = malloc(vert_n * sizeof(array));

  for (int i = 0; i < vert_n; ++i) {
    adj_list[i] = alloc_array(max_neighbour_n);
  }
  return adj_list;
}

void dealloc_adj_list(array adj_list[], int size) {
  for (int i = 0; i < size; ++i) {
    dealloc_array(adj_list[i]);
  }
  free(adj_list);
}

void load_edges(array adj_list[], int k) {
  while (k--) {
    int a, b;
    scanf("%d %d", &a, &b);
    append(&adj_list[a], b);
    append(&adj_list[b], a);
  }
}

/* GRAPH */
void BFS(int start_i, queue *q, array *visited, array *parents,
         array adj_list[], int n) {
  // for (int i = 0; i < n; ++i) {
  //   printf("visited %d: %d\n", i, visited->d[i]);
  // }
  put(q, start_i);
  while (!is_empty(q)) {
    int node = get(q);
    if (visited->d[node]) {
      continue;
    }
    visited->d[node] = 1;

    array neighbours = adj_list[node];
    for (int i = 0; i < neighbours.size; ++i) {
      int neighbour = neighbours.d[i];
      if (!visited->d[neighbour]) {
        parents->d[neighbour] = node;
        put(q, neighbour);
      }
    }
  }
}
int can_reach(array adj_list[], int n, int start, int end) {
  // max number of edges in complete graph
  queue q = alloc_queue(n * (n - 1) / 2);
  array visited = alloc_array(n);
  array parents = alloc_array(n);
  for (int i = 0; i < n; ++i) {
    append(&visited, 0);
    append(&parents, -1);
  }

  BFS(end, &q, &visited, &parents, adj_list, n);

  int current = start;
  int ret_val = 0;
  while (current != -1) {
    if (current == end) {
      ret_val = 1;
      break;
    }
    current = parents.d[current];
  }

  dealloc_array(visited);
  dealloc_array(parents);
  dealloc_queue(q);
  return ret_val;
}

void handle_searches(array adj_list[], int n, int p) {
  while (p--) {
    int a, b;
    scanf("%d %d", &a, &b);
    if (can_reach(adj_list, n, a, b)) {
      printf("YES\n");
    } else {
      printf("NO\n");
    }
  }
}

int main(void) {
  int n, k, p;
  scanf("%d %d %d", &n, &k, &p);
  array *adj_list = alloc_adj_list(n, k);
  load_edges(adj_list, k);

  handle_searches(adj_list, n, p);
  dealloc_adj_list(adj_list, n);
}
