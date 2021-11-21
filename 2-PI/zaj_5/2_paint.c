/*

2. Kolorowanka
1 Zadanie

Napisz program, który na wejściu otrzymuje kwadratową tablicę reprezentującą
częściowo pokolorowany obrazek (0 oznacza brak koloru) i wypełnia wskazane
niepokolorowane obszary podanymi kolorami. Program wczytuje współrzędne punktu
oraz kod koloru i maluje na ten kolor wszystkie pola sąsiednie (o wspólnej
krawędzi). Postępujemy tak do napotkania granicy planszy lub pola wcześniej
pokolorowanego.

1.1 Algorytm

Wykorzystaj kolejkę. W pętli pobierz jeden punkt z kolejki, koloruj go (jeśli
jeszcze nie ma koloru) i umieść w kolejce wszystkie sąsiednie pola (czyli takie,
które mają wspólną krawędź z bieżącym).

2 Wejście

W pierwszym wierszu standardowego wejścia znajdują się dwie liczby naturalne: n
(1 <= n <= 1000) - rozmiar tablicy kwadratowej i k (1 <= k <= 100) - liczba
początkowych pól do wypełnienia kolorem. Każdy z kolejnych n wierszy zawiera n
liczb naturalnych z zakresu [0, 1000] - początkowe kolory poszczególnych
pikseli. Następne k wierszy zawiera po 3 liczby każdy - współrzędne x i y
piksela, od którego należy zacząć kolorowanie oraz c - kolor, jakim należy
wypełnić cały dostępny pusty obszar (aż do napotkania innych kolorów lub brzegu
obrazka).

3 Wyjście

Na standardowym wyjściu programu powinno znaleźć się n wierszy po n liczb
oddzielonych spacją każdy - pokolorowany obrazek.

4 Przykład

4.1 Wejście

3 1
1 0 0
0 1 0
0 0 1
0 1 2

4.2 Wyjście

1 2 2
0 1 2
0 0 1

*/

#include <stdio.h>
#include <stdlib.h>

typedef unsigned short img_d;

typedef struct point {
  unsigned int x;
  unsigned int y;
  unsigned int color;
} point;

typedef struct queue {
  point *data;
  unsigned int start;
  unsigned int end;
  unsigned int size;
} queue;

void push_back(queue *const q, const point e) {
  q->data[(q->end++) % q->size] = e;
}

const point pop_front(queue *const q) {
  return q->data[(q->start++) % q->size];
}
const int is_empty(const queue *const q) { return q->start == q->end; }

queue alloc_queue(const int n) {
  return (queue){
      .start = 0, .end = 0, .size = n, .data = malloc(n * sizeof(point))};
}

void dealloc_queue(queue *q) { free(q->data); };

img_d **read_image(const unsigned int n) {
  img_d **image = malloc(n * sizeof(img_d *));
  for (size_t i = 0; i < n; ++i) {
    image[i] = malloc(n * sizeof(img_d));
  }

  for (size_t c = 0; c < n; ++c) {
    for (size_t r = 0; r < n; ++r) {
      scanf(" %hu", &image[c][r]);
    }
  }
  return image;
}

void print_image(img_d **array, const int n) {
  for (size_t c = 0; c < n; ++c) {
    for (size_t r = 0; r < n; ++r) {
      printf("%d ", array[c][r]);
    }
    printf("\n");
  }
}

void dealloc_image(img_d ***array, const int n) {
  for (size_t i = 0; i < n; ++i) {
    free((*array)[i]);
  }
  free(*array);
}

void read_filling_data(queue *q, int k) {
  while (k--) {
    unsigned int x, y, color;
    scanf(" %d %d %d", &y, &x, &color);
    push_back(q, (point){.x = x, .y = y, .color = color});
  }
}

#define DIRECTIONS_N 4
point DIRECTIONS[DIRECTIONS_N] = {
    {.x = 0, .y = 1},
    {.x = 1, .y = 0},
    {.x = 0, .y = -1},
    {.x = -1, .y = 0},
};

const int get_color(img_d **image, const point p) { return image[p.y][p.x]; }
void set_color(img_d **image, const point p) { image[p.y][p.x] = p.color; }

void add_neighbours_to_queue(queue *q, img_d **image, point p, const int n) {
  for (size_t i = 0; i < DIRECTIONS_N; ++i) {
    point new_p = {.x = p.x + DIRECTIONS[i].x,
                   .y = p.y + DIRECTIONS[i].y,
                   .color = p.color};

    // if in bounds & not colored
    if (new_p.x < 0 || new_p.x >= n || new_p.y < 0 || new_p.y >= n ||
        get_color(image, new_p) != 0) {
      continue;
    }
    push_back(q, new_p);
  }
}

int main(void) {
  unsigned int n, k;
  scanf("%d %d", &n, &k);
  queue q = alloc_queue(n * n);

  img_d **image = read_image(n);
  read_filling_data(&q, k);

  while (!is_empty(&q)) {
    const point p = pop_front(&q);

    // if not colored
    if (get_color(image, p) == 0) {
      set_color(image, p);
    }
    add_neighbours_to_queue(&q, image, p, n);
  }
  print_image(image, n);
  // cleanup
  dealloc_image(&image, n);
  dealloc_queue(&q);
}
