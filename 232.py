import numpy

race = numpy.zeros((100,100))
#race[99,99] = 1.0/3

def getrace(i, j):
    if i >= 100:
        return 0.0
    elif j >= 100:
        return 1.0
    else:
        return race[i, j]

def expectation(i, j, n):
    rest = ( 1.         / 2**(n+1) * getrace(i + 1, j + 2**(n-1))) + \
           ((2**n - 1.) / 2**(n+1) * getrace(i + 1, j + 0       )) + \
           ( 1.         / 2**(n+1) * getrace(i + 0, j + 2**(n-1)))
    x = rest / ((1 + 2**(-n)) / 2.)
    if i == 97 and j == 99:
        print i,j,n,rest,x
    return x
for i in range(99,-1,-1):
    for j in range(99,-1,-1):
        race[i,j] = max(expectation(i, j, n) for n in range(1,9))

print race
print '%.8f' % race[0,0]
