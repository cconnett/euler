int main() {
  unsigned long long fac = 1, i;
  for (i = 2; i <= 1000000000000l; i++) {
    fac %= 100000;
    fac *= i;
    while (fac % 10 == 0) {
      fac /= 10;
    }
    //printf("%ul\n", fac);
    //printf("%ul\n\n", fac);

    if (i % 1000000 == 0) {
      printf("%u\n", i/1000000);
    }
  }
  printf("%u\n", fac%100000);
}
