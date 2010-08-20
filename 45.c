int main() {
  unsigned long long t, a, b;
  for (t = 2;; t++){
    for (a = 1; a < t/2; a++) {
      b = t - a;
      if (isP(p(b) - p(a))) {
      }
    }
  }
}

unsigned long long p(unsigned long long n) {
  return (3*n*n-n)/2;
}

int isP(unsigned long long p) {
  
}
