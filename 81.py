from astar import astar, getpath

matrix = [eval('['+line.strip()+']') for line in file('matrix.txt').readlines()]

def successors(s):
    if s[0] == 'START':
        return [(r,0) for r in range(len(matrix))]
    (r,c) = s[0]

    succs = []
    if r+1 < len(matrix):
        succs.append((r+1,c))
    if c+1 < len(matrix[-1]):
        succs.append((r,c+1))
    return succs

def successors82(s):
    if s[0] == 'START':
        return [(r,0) for r in range(len(matrix))]
    (r,c) = s[0]

    succs = []
    if r+1 < len(matrix):
        succs.append((r+1,c))
    if c+1 < len(matrix[-1]):
        succs.append((r,c+1))
    if r-1 >= 0:
        succs.append((r-1,c))
    if c-1 >= 0:
        succs.append((r,c-1))
    return succs

def goal81(s):
    return s[0] == (len(matrix)-1,len(matrix[-1])-1)

def goal82(s):
    try:
        (r,c) = s[0]
    except ValueError:
        return False
    return c == len(matrix[0]) - 1

def getcost(spot):
    if spot == 'START':
        return 0
    return matrix[spot[0]][spot[1]]

def g(state):
    #print getpath(state)
    return sum(getcost(spot) for spot in getpath(state))

min_entry = min(min(matrix[r][c] for c in range(len(matrix[r])))
                for r in range(len(matrix)))
print 'Min entry:', min_entry
def h(((r,c),last)):
    return (abs(r - (len(matrix)-1)) + abs(c - (len(matrix[-1])-1)))

def h82((spot,last)):
    if spot == 'START':
        return min_entry*len(matrix[0])
    (r,c) = spot
    return min_entry*len(matrix[r]) - 1 - c

# p81
#print astar((0,0), successors, goal81, g, h)
# p82
#print astar('START', successors82, goal82, g, h82)
# p83
print astar((0,0), successors82, goal81, g, h)
