import re
import primes
import itertools
import nometer
import sys

meter = nometer.nometer()
#meter.autoreport()

width = 6
target = 8
subbits = 3
myprimes = list(itertools.takewhile(lambda n: n < 10**width, primes.primes()))
pstrs = [str(p) for p in myprimes if len(str(p)) == width]

masks = range(1,(1 << width) - 1)

def pstrAndMaskToRegex(pstr, mask):
    regex = ''
    subbed = 0
    first = True
    position = width - 1
    while position >= 0:
        testbit = 1 << position
        if mask & testbit != 0:
            subbed += 1
            if first:
                regex += '(.)'
                first = False
            else:
                regex += r'(?:\1)'
        else:
            regex += pstr[width - 1 - position]
        position -= 1
    if subbed != subbits:
        return None
    return ''.join(regex)

tried = set()

#masks = [6]
print len(masks), 'masks to try.'
for mask in masks:
    print 'Using mask', mask
    for pstr in pstrs:
        regex = pstrAndMaskToRegex(pstr, mask)
        if regex is None:
            continue
        if regex in tried:
            continue
        tried.add(regex)
        rx = re.compile(regex)
        print regex
        matches = [x for x in pstrs if rx.match(x)]
        if len(matches) >= target:
            print matches[0], regex, matches
            sys.exit(0)
        

