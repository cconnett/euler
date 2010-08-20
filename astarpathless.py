from PriorityQueue import PriorityQueue
import sys

def astar(start, successors, goal, g, h):
    path = []
    states = PriorityQueue()
    firststate = start
    states.put((-g(firststate) - h(firststate), firststate))
    
    i = 0
    nexti = 1
    while True:
        i += 1
        #if i == 40000:
        #    return (0, None)
        if i == nexti:
            nexti += 1 # 3*len(states)
            curbest = states.top()[1]
            print '%8d: # states: %5d, h of best state: %5d' % \
                  (i, len(states), h(curbest))
            print '\tcurrent top: score = %6d; %r' % (g(curbest) + h(curbest),
                                                      curbest)
        try:
            if goal(states.top()[1]):
                print i
                return (states.top()[1], g(states.top()[1]))
        except IndexError:
            return (0, None)
        best = states.top()[1]
        states.pop()
        for s in successors(best):
            prev = [x for x in states if x[1] == s]
            #if len(prev)>0:print prev[0][1]
            #print s
            if len(prev) > 0 and g(s) < g(prev[0][1]):
                states.remove(prev[0])
            if len(prev) > 0:
                continue
            states.put((-g(s) - h(s), s))
