/*
1 Zadanie

Mamy do zainstalowania kilka pakietów oprogramowania. Dysponujemy grafem
skierowanym definiującym zależności między pakietami. Jeśli krawędź prowadzi od
pakietu A do pakietu B oznacza to, że pakiet A musi zostać zainstalowany przed
pakietem B. Napisz program, który dla zadanego grafu skierowanego stwierdzi, czy
możliwe jest ustalenie takiej kolejności instalacji aby spełnione były wszystkie
zależności między pakietami. Jeżeli takie uporządkowanie istnieje to wypisuje
przykładową sekwencję instalacji.

Uwaga: Ten program można napisać w C++ z wykorzystaniem kontenerów biblioteki
STL.

2 Wejście

Pierwsza linia zawiera dwie liczby całkowite n, k będące odpowiednio liczbą
wierzchołków i liczbą krawędzi grafu. W kolejnych k wierszach znajdują się opisy
krawędzi złożone z dwóch liczb 0 <= a, b <= n − 1 oznaczających, że graf zawiera
krawędź z a do b.

3 Wyjście

Pierwsza linia standardowego wyjścia zawiera dokładnie jedno słowo: OK jeśli
odpowiednia kolejność istnieje (graf jest acykliczny) lub CYCLE w przeciwnym
przypadku. Dla grafu acyklicznego wyjście zawiera dodatkową linię złożoną z n
liczb całkowitych - przykładową kolejność instalacji pakietów.
*/

#include <stdio.h>
#include <stdlib.h>

typedef struct node {
  int is_visited;
  int is_in_visited_branch;
} node;

void DFS(node **vertices, int n, int **adj_matrix, int start_i, int parent,
         int *has_cycle, int *order, int *order_i) {
  vertices[start_i]->is_in_visited_branch = 1;
  vertices[start_i]->is_visited = 1;

  for (int i = 0; i < n; ++i) {
    if (adj_matrix[start_i][i]) {
      if (vertices[i]->is_in_visited_branch) {
        *has_cycle = 1;
        return;
      }
      if (!vertices[i]->is_visited) {
        DFS(vertices, n, adj_matrix, i, start_i, has_cycle, order, order_i);
      }
    }
  }
  order[--*order_i] = start_i;
  vertices[start_i]->is_in_visited_branch = 0;
}

int main(void) {
  int n, k;
  scanf("%d %d", &n, &k);
  // alloc
  int **adj_matrix = malloc(n * sizeof(int *));
  for (int i = 0; i < n; ++i) {
    adj_matrix[i] = calloc(n, sizeof(int));
  }
  node **vertices = malloc(n * sizeof(node *));
  for (int i = 0; i < n; ++i) {
    node *vertex = malloc(sizeof(node));
    *vertex = (node){
        .is_visited = 0,
        .is_in_visited_branch = 0,
    };
    vertices[i] = vertex;
  }

  int order_i = n;
  int *order = malloc(n * sizeof(int));

  // read input
  for (int i = 0; i < k; ++i) {
    int a, b;
    scanf("%d %d", &a, &b);
    adj_matrix[a][b] = 1;
  }

  // DFS
  int has_cycle = 0;
  for (int i = 0; i < n; ++i) {
    if (vertices[i]->is_visited) {
      continue;
    }

    DFS(vertices, n, adj_matrix, i, -1, &has_cycle, order, &order_i);
    if (has_cycle) {
      printf("CYCLE\n");
      goto dealloc;
    }
  }

  // print results
  printf("OK\n");

  for (int i = 0; i < n; ++i) {
    printf("%d ", order[i]);
  }
  printf("\n");

// dealloc
dealloc:
  for (int i = 0; i < n; ++i) {
    free(adj_matrix[i]);
  }
  free(adj_matrix);
  for (int i = 0; i < n; ++i) {
    free(vertices[i]);
  }
  free(vertices);
  free(order);
}