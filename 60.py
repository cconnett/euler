from astarpathless import astar
from primes import primes, isPrime, nextPrime

N = 4
def compatible(primeset, newprime):
    newprime = str(newprime)
    primeset = map(str, primeset)
    return all(isPrime(int(newprime+oldprime)) for oldprime in primeset) and\
           all(isPrime(int(oldprime+newprime)) for oldprime in primeset)

def successors((primeset, newprime)):
    nextprime = nextPrime(newprime) #primes(newprime).next()
    succs = [(primeset, nextprime)]
    if compatible(primeset, newprime):
        succs.append((primeset+[newprime], nextprime))
    return succs

def goal((primeset, newprime)):
    #print (primeset, newprime)
    return len(primeset) >= N
def g((primeset, newprime)):
    return sum(primeset)
def h((primeset, newprime)):
    return (N-len(primeset))*newprime

print astar(([],3), successors, goal, g, h)
