#include <math.h>
#include <stdio.h>
#include <gmp.h>

#define BILLION 1000000000ul

int main() {
  mpz_t cand;
  mpz_init(cand);
  long i;
  unsigned int bitmask;
  char buf[10];
  char *p;
  
  for (mpz_set_ui(cand, BILLION-1), i=BILLION-1; i>=0;
       mpz_sub_ui(cand, cand, 2),i-=2) {
    if (mpz_probab_prime_p(cand, 12)) {
      bitmask = 0x01;
      sprintf(buf, "%d", i);
      p = buf;
      int boolBroken = 0;
      while(*p) {
        if (bitmask & (1<<((*p)-'0'))) {
          boolBroken = 1;
          break;
        }
        bitmask |= (1<<((*p)-'0'));
        p++;
      }
      if (boolBroken) {
        goto loop;
      } else if ((bitmask & (bitmask + 1)) == 0) {
        printf("Answer: %d\n", i, bitmask);
        goto done;
      }
        
    }
  loop:
    if (i%100000 == 99999) {
      printf("Checkpoint: %d\n", i);
    }
  }
 done:
  mpz_clear(cand);
}
