#include <math.h>
#include <stdio.h>
#include <gmp.h>

int main() {
  unsigned long long ans = 0;
  unsigned long a, b;
  signed int mod;
  unsigned long s2;

  mpz_t ta, q, r;
  mpz_init(ta);
  mpz_init(q);
  mpz_init(r);
  
  for (a = 1; a <= 333333334; a++) {
    for (mod = -1; mod <= 1; mod += 2) {
      b = a + mod;
      s2 = a+a+b;

      mpz_set_ui(ta, s2);
      mpz_mul_ui(ta, ta, s2-2*a);
      mpz_mul_ui(ta, ta, s2-2*a);
      mpz_mul_ui(ta, ta, s2-2*b);

      if (mpz_cmp_ui(ta, 0) > 0 &&
          mpz_perfect_square_p(ta)) {
        printf("%d %d\n", a, b);
        ans += a+a+b;
      }
    }
  }
  printf("%d\n", ans);

  mpz_clear(ta); 
  mpz_clear(q);
  mpz_clear(r);
}
