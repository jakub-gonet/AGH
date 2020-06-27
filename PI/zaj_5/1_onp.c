/*
1. Kalkulator

1 Zadanie

Napisz program obliczający wartość wyrażenia zapisanego w ONP.
(https://pl.wikipedia.org/wiki/Odwrotna_notacja_polska).

2 Wejście

W pierwszym wierszu standardowego wejścia znajdują się dwie liczby naturalne: n
(1 <= n <= 1000) - liczba operandów (liczb) w podanym wyrażeniu oraz k (1 <= k
<= 8)
- maksymalna długość każdej liczby w zapisie dziesiętnym. W drugim wierszu
znajduje się wyrażenie zapisane w formie ONP, zawierające n liczb całkowitych, w
którym wszystkie operandy i operatory oddzielone są białymi znakami. Można
założyć, że wyrażenie jest poprawne.

3 Wyjście

Na standardowym wyjściu programu powinna znaleźć się jedna liczba całkowita -
obliczona wartość wyrażenia.

4 Przykład
4.1 Wejście

3 2
2 2 2 + *

4.2 Wyjście

8
1
*/

#include <stdio.h>
#include <stdlib.h>

#define MAX_K 8
#define MAX_N 1000

typedef long long number;
typedef const number (*op_f)(const number a, const number b);

typedef struct stack {
  number stack[MAX_N];
  unsigned int size;
} stack;

const number add(const number a, const number b) { return a + b; }
const number subtract(const number a, const number b) { return a - b; }
const number multiply(const number a, const number b) { return a * b; }
const number divide(const number a, const number b) { return a / b; }

void push(stack *s, const number n) { s->stack[s->size++] = n; }
const number pop(stack *s) { return s->stack[--(s->size)]; }
void apply_f(stack *s, op_f f) {
  const number a = pop(s);
  const number b = pop(s);
  push(s, f(b, a));
}

int main(void) {
  stack s = {.stack = {}, .size = 0};

  int n;
  // ignore k, we have static bufor of upper bound value for it
  scanf("%d %*d", &n);
  // num of ops, given that every op is of arity 2
  n = 2 * n - 1;
  while (n--) {
    char buf[MAX_K + 1];
    scanf(" %s", buf);

    switch (buf[0]) {
      case '+':
        apply_f(&s, add);
        break;
      case '-':
        if (buf[1] != '\0') {
          // input allows integers so we need to check if current thing is `-`
          // op or negative number
          // we could use conditional break, but local goto is more readable
          goto negative_num;
        }
        apply_f(&s, subtract);
        break;
      case '*':
        apply_f(&s, multiply);
        break;
      case '/':
        apply_f(&s, divide);
        break;

      // not an operation, must be number
      default:
      negative_num:
        push(&s, atoll(buf));
        break;
    }
  }
  printf("%lld\n", pop(&s));
}