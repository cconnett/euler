#include <math.h>
#include <stdio.h>
#include <gmp.h>

int main() {
  int d;
  mpz_t y, x;
  mpz_init(y);
  mpz_init(x);
  
  
  for (d = 1; d <= 1000; d++) {
    mpz_set_ui(x, d);
    if (mpz_perfect_square_p(x)){
      continue;
    }
    
    printf("%d: ", d);
    fflush(stdout);
    for (mpz_set_ui(y, 1); ; mpz_add_ui(y, y, 1)) {
      mpz_mul(x, y, y); // x = y^2
      mpz_mul_ui(x, x, d); // x *= d
      mpz_add_ui(x, x, 1); // x++;
      
      if (mpz_perfect_square_p(x)) {
        mpz_out_str(stdout, 10, x);
        printf("\n");
        break;
      }
    }
  }
  
  mpz_clear(y);
  mpz_clear(x);
}
