#include <stdio.h>
#include <limits.h>

#define MAX_STACK 12

int stack[MAX_STACK];
int mins[201];

int search(int top) {
  int nextPower;
  for (nextPower = stack[top-1] + 1; nextPower <= 200; nextPower++) {
    int possible = 0;
    int *addend1, *addend2;
    for (addend1 = stack; addend1 < stack + top; addend1++) {
      for (addend2 = addend1; addend2 < stack + top; addend2++) {
        if (*addend1 + *addend2 == nextPower) {
          possible = 1;
          goto endsearch;
        }
      }
    }
  endsearch:
    if (possible) {
      stack[top] = nextPower;
      if (top < mins[nextPower]) {
        mins[nextPower] = top;
        printf("New min found for %3d: %2d\n", nextPower, top);
        int k4;
        for (k4 = 1; k4 <= 197; k4 += 4) {
          int nybble = 0;
          nybble |= (mins[k4+0] < INT_MAX) << 3;
          nybble |= (mins[k4+1] < INT_MAX) << 2;
          nybble |= (mins[k4+2] < INT_MAX) << 1;
          nybble |= (mins[k4+3] < INT_MAX) << 0;
          printf("%x", nybble);
        }
        printf("\n");
      }
      if (top < MAX_STACK - 1) {
        search(top + 1);
      }
    }
  }
}

int main() {
  stack[0] = 1;
  stack[1] = 2;
  mins[1] = 0;
  mins[2] = 1;
  
  int k;
  for (k = 3; k <= 200; k++) {
    mins[k] = INT_MAX;
  }

  search(2);

  int sum = 0;
  for (k = 1; k <= 200; k++) {
    printf("Min for %3d: %2d\n", k, mins[k]);
    sum += mins[k];
  }

  printf("Answer: %d\n", sum);
}
