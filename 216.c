#include <math.h>
#include <stdio.h>
#include <gmp.h>

int main() {
  mpz_t cand;
  mpz_init(cand);
  int n;
  int count = 0;

  for (n = 2; n <= 50000000; n++) {
    mpz_set_ui(cand, n);
    mpz_mul(cand, cand, cand);
    mpz_mul_ui(cand, cand, 2);
    mpz_sub_ui(cand, cand, 1);

    if (mpz_probab_prime_p(cand, 12)) {
      printf("%d\n", n);
      count++;
    }
  }
  printf("Answer: %d\n", count);
  mpz_clear(cand);
}
