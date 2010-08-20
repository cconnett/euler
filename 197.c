#include <stdio.h>
#include <math.h>

int main() {
  double v = -1.0, prevv;
  for (long i = 1; i <= 1000000000001; i++) {
    prevv = v;
    v = floor(pow(2,30.403243784 - v * v)) * 1e-9;
    if (i % 100000 == 0) {
      printf("%d, %.10f %.10f\n", i, prevv, v);
    }
  }
}
