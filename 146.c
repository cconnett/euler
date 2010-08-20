#include <gmp.h>
#include <stdio.h>

int main() {

  mpz_t n, n2, np, d, q, res;
  mpz_init(n);
  mpz_init(n2);
  mpz_init(np);
  mpz_init(d);
  mpz_init(q);
  mpz_init(res);
  mpz_set_ui(res, 0);
  
  for (mpz_set_ui(n, 10); mpz_cmp_ui(n, 150000000) < 0; mpz_add_ui(n, n, 10)) {
    mpz_mul(n2, n, n);
    mpz_set(np, n2);
    
    mpz_nextprime(np, np);
    mpz_sub(d, np, n2);
    if (mpz_cmp_ui(d, 1) != 0) continue;
    mpz_nextprime(np, np);
    mpz_sub(d, np, n2);
    if (mpz_cmp_ui(d, 3) != 0) continue;
    mpz_nextprime(np, np);
    mpz_sub(d, np, n2);
    if (mpz_cmp_ui(d, 7) != 0) continue;
    mpz_nextprime(np, np);
    mpz_sub(d, np, n2);
    if (mpz_cmp_ui(d, 9) != 0) continue;
    mpz_nextprime(np, np);
    mpz_sub(d, np, n2);
    if (mpz_cmp_ui(d, 13) != 0) continue;
    mpz_nextprime(np, np);
    mpz_sub(d, np, n2);
    if (mpz_cmp_ui(d, 27) != 0) continue;

    mpz_add(res, res, n);
    mpz_out_str(stdout, 10, n);
    printf("\n");
  }

  mpz_out_str(stdout, 10, res);
  printf("\n");
  
  mpz_clear(n);
  mpz_clear(n2);
  mpz_clear(np);
  mpz_clear(d);
  mpz_clear(q);
  mpz_clear(res);
  return 0;
}
