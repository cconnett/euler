from fractions import Fraction
from pprint import pprint

c = {Fraction(60,1):1}

def par(a,b):
    return a+b
def seq(a,b):
    a = Fraction(a.denominator, a.numerator)
    b = Fraction(b.denominator, b.numerator)
    recip = a + b
    return Fraction(recip.denominator, recip.numerator)

newc = {}
while True:
    for (a,na) in c.items():
    #for (a,na) in [(x, nx) for (x, nx) in c.items() if nx == n]:
        for (b,nb) in c.items():
            if na + nb <= 10:
                v = par(a,b)
                if v not in c:
                    newc.setdefault(v, 100000)
                    newc[v] = min(newc[v], na + nb)
                v = seq(a,b)
                if v not in c:
                    newc.setdefault(v, 100000)
                    newc[v] = min(newc[v], na + nb)
    if not newc:
        break
    newc.update(c)
    c = newc
    newc = {}
    #print n, max(c.values())

def v(x):
    return x[1]
#print(list(sorted(c.items(), key=v)))
#print(len(c))
s = set(c.keys())
print(s, file=open('cap10.txt','w'))
print(len(s))
