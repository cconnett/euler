from fractions import Fraction
from pprint import pprint

c = {1:set([Fraction(60,1)])}

def par(a,b):
    return a+b
def seq(a,b):
    a = Fraction(a.denominator, a.numerator)
    b = Fraction(b.denominator, b.numerator)
    recip = a + b
    return Fraction(recip.denominator, recip.numerator)

allcaps = set(c[1])
for newcount in range(2,10+1):
    newcaps = set()
    for na in range(1,newcount//2+1):
        nb = newcount - na
        for a in c[na]:
            for b in c[nb]:
                v = par(a,b)
                if v not in allcaps and v not in newcaps:
                    newcaps.add(v)
                v = seq(a,b)
                if v not in allcaps and v not in newcaps:
                    newcaps.add(v)
    #print(newcount, newcaps)
    allcaps |= newcaps
    c[newcount] = newcaps

def v(x):
    return x[1]
#print(list(sorted(c.items(), key=v)))
print(allcaps, file=open('cap10b.txt','w'))
print(len(allcaps))
