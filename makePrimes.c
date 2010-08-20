#include <gmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  mpz_t p;
  mpz_init(p);
  char *pstr = malloc(20);
  FILE *f = fopen("billprimes2.txt","w");
  while (mpz_cmp_ui(p, 1000000000) < 0) {
    mpz_nextprime(p, p);
    mpz_get_str(pstr, 10, p);
    if (strchr(pstr, '0') == NULL) {
      char digit;
      int good = 1;
      for (digit = '0'; digit <= '9'; digit++) {
        //printf("%s %c %s %s\n",pstr,digit, strchr(pstr,digit), strrchr(pstr,digit));
        if (strchr(pstr, digit) != NULL &&
            strchr(pstr, digit) != strrchr(pstr, digit)) {
          good = 0;
          //printf("bad\n");
          break;
        }
      }
      if (good) {
        //mpz_out_str(stdout, 10, p);
        //printf("\n");

        mpz_out_str(f, 10, p);
        fprintf(f, "\n");
      }
    }
  }
  mpz_clear(p);
}
