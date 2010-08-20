import math

def factor(n):
    if n <= 1:
        return
    for p in [2] + list(range(3, math.floor(math.sqrt(n))+1, 2)):
        if n % p == 0:
            yield p
            for f in factor(n // p):
                yield f
            break
    else:
        yield n

def thesePrimes(limit):
    divlist = [7]
    yield 7
    for n in range(2,limit+1):
        t = 2*n*n-1
        for d in divlist:
            if t % d == 0:
                break
        else:
            if len(list(factor(t))) == 1:
                yield t
            divlist = list(sorted(set(divlist + list(factor(t)))))

def main(limit):
    numPrimes = 0
    for p in thesePrimes(limit):
        print(p)
        numPrimes += 1
    print('Answer: %d' % numPrimes)
