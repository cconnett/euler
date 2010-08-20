#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gmp.h>

int main() {
  int i;
  mpz_t s, r, ticker;
  mpz_init(r);
  mpz_init(s);
  mpz_init(ticker);
  mpz_set_ui(ticker, 10000000);
  char *tick_str = malloc(9);
  char *s_str = malloc(20);
  memcpy(s_str, "1020304050607080900", 20);

  while (1) {
    mpz_get_str(tick_str, 10, ticker);
    for (i = 1; i <= 15; i += 2) {
      s_str[i] = tick_str[i/2];
    }
    //mpz_out_str(stdout, 10, ticker);
    //printf("\n");
    mpz_set_str(s, s_str, 10);
    //mpz_out_str(stdout, 10, s);
    //printf("\n");

    if (mpz_perfect_square_p(s)) {
      mpz_out_str(stdout, 10, s);
      printf(" ");
      mpz_sqrt(r, s);
      mpz_out_str(stdout, 10, r);
      printf("\n");
    }

    mpz_add_ui(ticker, ticker, 2);
  }

  mpz_clear(s);
  mpz_clear(r);
  mpz_clear(ticker);
}
