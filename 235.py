import numpy

race = numpy.zeros((100,100))
race[99,99] = 1.0/3

def expectation(i, j, n):
    return  1.            / 2**(n+1) * race[i + 1, j + 2**(n-1)] + \ # heads success
           (2**(n-1) - 1) / 2**(n+1) * race[i + 1, j + 0       ] + \ # heads failure
            1.            / 2**(n+1) * race[i + 0, j + 2**(n-1)] + \ # tails success
           (2**(n-1) - 1) / 2**(n+1) * race[i + 0, j + 0       ]     # tails failure
for i in range(99,-1,-1):
    for j in range(99,-1,-1):
        race[i,j] = max(expectation(n) for n in range(1,9))

print race
