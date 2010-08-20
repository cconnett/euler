#include <math.h>
#include <stdio.h>
#include <gmp.h>

#define tenmil 10000000

int ea89[tenmil];

int main() {
  int i;
  for (i = 0; i < tenmil; i++) {
    ea89[i] = 1;
  }
  ea89[89] = 2;
  ea89[1] = 0;

  int count = 0;
  for (i = 1; i < tenmil; i++) {
    if (f(i) == 2) {
      //printf("%d\n", i);
      count++;
    }
  }

  printf("%d\n", count);
}

int f(int n) {
  if (ea89[n] == 1) {
    ea89[n] = f(sqd(n));
  }
  //fprintf(stderr, "%d->%d\n", n, ea89[n]);
  return ea89[n];
}

int sqd(int n) {
  char buf[10000];
  sprintf(buf, "%d", n);
  char *p = buf;
  int sum = 0;
  while (*p){
    sum += ((*p)-'0') * ((*p)-'0');
    p++;
  }
  //fprintf(stderr, "%d->%d\n", n, sum);
  return sum;
}
