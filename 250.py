counts = [0]*250
counts[0] = 1

ten_to_the_sixteenth = 10**16

for n in xrange(1, 250250 + 1):
    if n%1000 == 0:
        print n
    more = [0] * 250
    for r in xrange(250):
        more[(r + pow(n, n, 250)) % 250] += counts[r]
    for r in xrange(250):
        counts[r] += more[r]
        counts[r] %= ten_to_the_sixteenth
print counts[0] - 1
