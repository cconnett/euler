#include <stdio.h>

int reverse(int n) {
  int r = 0;
  while (n) {
    r *= 10;
    r += n % 10;
    n /= 10;
  }
  return r;
}

int main() {
  int i = 1;
  int sum;
  int reversible = 0;
  int count = 0;
  for (i = 1; i < 1000000000; i++) {
    if (i % 10 == 0) {
      continue;
    }
    
    sum = i + reverse(i);
    reversible = 1;
    while (sum) {
      if (sum % 2 == 0) {
        reversible = 0;
        break;
      }
      sum /= 10;
    }
    if (reversible) {
      count++;
      //printf("%d\n", i);
    }

    if (i % 1000000 == 999999) {
      printf("%d / %d\n", count, i);
    }
  }
}
