/*
1. Szyfr Cezara - modyfikacja
1 Zadanie
Napisz program, który szyfruje zadany tekst używając zmodyfikowanego szyfru
Cezara. Zasada działania szyfru jest następująca: każda litera tekstu
przesunięta jest o stałą liczbę liter “w prawo”. Nazwijmy ją przesunięciem i
oznaczmy s. Np. dla wartości s = 3 słowo “ala” przekształca się w słowo “dod”.
Przesunięcia są obliczane na zasadzie modulo, zatem litera z przesunięta o 3
stanie się literą c. Nasza modyfikacja polega na założeniu, że wartość
przesunięcia s jest równa długości pierwszego słowa tekstu. Spacje nie są
szyfrowane. Uwaga: W systemie dostępna jest templatka programu do wykorzystania
(w zakładce Pliki).

2 Wejście
Pierwszy i jedyny wiersz standardowego wejścia zawiera pewną liczbę słów
oddzielonych spacją. Każde ze słów składa się wyłącznie z małych lub dużych
liter alfabetu angielskiego. Długość linii nie przekracza 100 znaków. W tekście
dopuszczalne są wiodące spacje.

3 Wyjście
Na standardowym wyjściu programu powinien znaleźć się jeden wiersz zawierający
zaszyfrowany tekst.

4 Przykład Wiadomość
“to be or not to be” zostanie zaszyfrowana do napisu “vq dg qt pqv vq dg”.
*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define N 101  //	100 chars + nullbyte

int first_word_length(char* message) {
  int k = 0;
  while (isalpha(*message++)) {
    ++k;
  }
  return k;
}

int shift_lowercase_letter(const char letter, const int K) {
  const char letter_n = letter - 'a';
  return 'a' + (letter_n + K) % 26;
}

char* skip_leading_spaces(const char* message) {
  while (*message == ' ') {
    ++message;
  }
  return message;
}

void encrypt(char* message) {
  message = skip_leading_spaces(message);
  const int K = first_word_length(message);

  // assuming null-terminated string
  for (; *message != '\0'; ++message) {
    char letter = *message;
    // if not in a-z, A-Z
    if (!isalpha(letter)) {
      continue;
    }
    const int is_upper = isupper(letter);
    letter = tolower(letter);

    const char shifted = shift_lowercase_letter(letter, K);
    *message = is_upper ? toupper(shifted) : shifted;
  }
}

int main() {
  char message[N];
  fgets(message, N, stdin);
  encrypt(message);
  printf("%s\n", message);
}
