#include <stdio.h>
#include <gmp.h>
#include <stdlib.h>

int bottomPD(mpz_t *n) {
  int place;
  int bitarray = 0;
  mpz_t tmp;
  mpz_init(tmp);
  mpz_set(tmp, *n);
  
  for (place = 0; place < 9; place++) {
    int digit = mpz_tdiv_ui(tmp, 10);
    if (digit != 0 && (bitarray & 1 << digit) == 0) {
      bitarray |= 1 << digit;
    } else {
      mpz_clear(tmp);
      return 0;
    }
    mpz_tdiv_q_ui(tmp, tmp, 10);
  }
  mpz_clear(tmp);
  return bitarray == 0x3fe;
}

int topPD(mpz_t *n) {
  int place;
  int bitarray = 0;
  //mpz_t tmp;
  //mpz_init(tmp);
  //mpz_set(tmp, *n);

  char *str = mpz_get_str(NULL, 10, *n);
  
  //int numDigits = mpz_sizeinbase(tmp, 10);
  //while (numDigits > 9) {
  //  mpz_tdiv_q_ui(tmp, tmp, 10);
  //  numDigits--;
  //}
  
  for (place = 0; place < 9; place++) {
    int digit = str[place]-'0'; //mpz_tdiv_ui(tmp, 10);
    if (digit != 0 && (bitarray & 1 << digit) == 0) {
      bitarray |= 1 << digit;
    } else {
      free(str);
      return 0;
    }
  }
  free(str);
  return bitarray == 0x3fe;
}

int main() {
  mpz_t n0, n1, n2;
  mpz_init_set_ui(n0, 2);
  mpz_init_set_ui(n1, 1);
  mpz_init_set_ui(n2, 1);

  int n = 3;
  while(1) {
    mpz_add(n2, n0, n1);
    mpz_swap(n2, n1);
    mpz_swap(n0, n1);
    n++;

    if (n < 297914) continue;

    int bpd = bottomPD(&n0);
    int tpd = topPD(&n0);
    
    if (bpd) {
      printf("BottomPD: %d\n", n);
    }
    if (tpd) {
      printf("TopPD: %d\n", n);
    }

    if (bpd && tpd) {
      printf("Answer: %d\n", n);
      return;
    }
  }
}
