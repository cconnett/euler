from memorize import memorize
import cPickle
import sys

#generate all length-32-layers
layers = set([()])
finallayers = set()
while True:
    nextlayers = []
    for layer in layers:
        layer = list(layer)
        s = sum(layer)
        if s == 32:
            finallayers.add(tuple(layer))
        if s <= 30:
            nextlayers.append(tuple(layer+[2]))
        if s <= 29:
            nextlayers.append(tuple(layer+[3]))
    layers = set(nextlayers)
    if len(layers) == 0:
        break
print len(finallayers)

@memorize
def compatible(a,b):
    a = list(a)
    b = list(b)
    ap = a[0]
    a.pop(0)
    bp = 0

    while len(a) > 0 and len(b) > 0:
        if ap == bp:
            return False
        if ap < bp:
            ap += a.pop(0)
        if ap > bp:
            bp += b.pop(0)
    return True

#try:
#    compatible.cache = cPickle.load(file('215.cache'))
#except:
#    pass

level = 1
tops = dict.fromkeys(finallayers, 1)
while level < 10:
    print 'Computing level %d' % level
    newtops = dict.fromkeys(finallayers, 0)
    for (i,top) in enumerate(tops):
        for layer in finallayers:
            if compatible(top,layer):
                newtops[layer] += tops[top]
        if level == 1:
            sys.stdout.write('%d    \r'%i)
            sys.stdout.flush()
#    cPickle.dump(compatible.cache,file('215.cache','w'),2)
    tops = newtops
    level += 1
    print tops.values()
print sum(tops.values())
