from PriorityQueue import PriorityQueue
import sys

def astar(start, successors, goal, g, h):
    path = []
    states = PriorityQueue()
    firststate = (start,None)
    states.put((-g(firststate) - h(firststate), firststate))
    
    i = 0
    while True:
        i += 1
        if i % 2500 == 0:
            print '%8d: # states: %5d, h of best state: %5d' % \
                  (i, len(states), min(h(state[1]) for state in states))
        try:
            if goal(states.top()[1]):
                return g(states.top()[1]), getpath(states.top()[1])
        except IndexError:
            return None
        best = states.top()[1]
        states.pop()
        for s in successors(best):
            prev = [x for x in states if x[1][0] == s]
            s = (s, best)
            #if len(prev)>0:print prev[0][1]
            #print s
            if len(prev) > 0 and g(s) < g(prev[0][1]):
                states.remove(prev[0])
            if len(prev) > 0:
                continue
            states.put((-g(s) - h(s), s))
    
def getpath(state):
    path = [state[0]]
    while state[1] is not None:
        state = state[1]
        path.append(state[0])
    path.reverse()
    return [state for state in path]
