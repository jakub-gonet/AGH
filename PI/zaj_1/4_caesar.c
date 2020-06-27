/*
Szyfr Cezara polega na szyfrowaniu kolejnych liter (pozostałe znaki pozostawiamy
bez zmian). Każda litera zostaje zamieniona w k-tą następną w alfabecie (k jest
stałą szyfru), przy czym jeżeli taka nie istnieje (wychodzimy za 'z'), to
odliczanie jest kontynuowane z powrotem od 'a'. Szyfrowanie zachowuje wielkość
liter. Napisz funkcję, która szyfruje ciąg znaków podany jako argument.
*/
#include <stdio.h>
#include <stdlib.h>

void wrap_char(char *const base_c, const int shift) {

  const char letter = (*base_c + shift);

  if (letter > 'z') { // we assume that 'z' has greater value than 'Z'
    const int diff = ('z' - (int)(*base_c));
    *base_c = 'a' + (shift - diff - 1);
  } else if (letter > 'Z') {
    const int diff = ('Z' - (int)(*base_c));
    *base_c = 'A' + (shift - diff - 1);
  }
}

void caesar_enc(char str[]) {
  const int K = 26;

  // NULL terminated
  for(;*str != '\0'; str++){
    const char letter = (*str + K);
    if(!(('a' <= *str && *str <= 'z') || ('A' <= *str && *str <= 'Z'))) {continue;}

    if (('a' <= letter && letter <= 'z') || ('A' <= letter && letter <= 'Z')) {
      *str = letter;
    } else {
      wrap_char(str, K);
    }
  }
}

int main(void) {
  char to_enc[] = "abcz123";
  caesar_enc(to_enc);
  // buffer overflow
  printf("%s\n", to_enc);
}
