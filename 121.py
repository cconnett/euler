# -*- encoding: utf-8 -*-
from random import random
from pprint import pprint

plays = 0
wins = 0

def play(rounds):
    red = 0
    blue = 0
    for t in range(2,2+rounds):
        if random() < 1.0/t:
            red += 1
        else:
            blue += 1
    #print red, blue
    return red > blue

#while True:
#    plays += 1
#    wins += play(15)
#    
#    if plays % 1000 == 0 and wins != 0:
#        #print wins, plays
#        print '£%d' % (int(float(plays)/wins))

p = {0:1.0}
for play in range(2,2+4):
    newp = {}
    for pos in p:
        newp.setdefault(pos-1,0)
        newp.setdefault(pos+1,0)
        newp[pos-1] += p[pos]*float(play-1)/play
        newp[pos+1] += p[pos]*1.0/play
    p = newp
print 1/sum(prob for (pos,prob) in list(sorted(p.items()))
            if pos > 0)
