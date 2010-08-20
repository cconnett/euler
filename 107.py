from copy import deepcopy as copy
matrix = [[int(elt) if elt.strip() != '-' else None
           for elt in line.split(',')]
          for line in file('network.txt').readlines()]

edges = []
for i in range(len(matrix)):
    for j in range(len(matrix[i])):
        if i < j and matrix[i][j] is not None:
            edges.append( (matrix[i][j], (i,j)) )

edges.sort()

graph = dict.fromkeys(range(len(matrix)))
for i in graph:
    graph[i] = list()

def bfs(graph, node=None, last=None, visited=None):
    if visited is None:
        visited = set()
    if node is None:
        for start in graph:
            if start not in visited:
                if bfs(graph, start):
                    return True
        return False

    visited.add(node)
    for neighbor in graph[node]:
        if neighbor is last:
            continue
        if neighbor in visited:
            return True
        if bfs(graph, neighbor, node, visited):
            return True
    return False

total = 0
for (w, (i,j)) in edges:
    g = copy(graph)
    g[i].append(j)
    g[j].append(i)
    if not bfs(g):
        graph = g
        total += w


#print 'graph {'
#for i in graph:
#    for j in graph[i]:
#        if i < j:
#            print i,'--',j,';'
#print '}'

print bfs(graph)
print graph
print total
maxweight = sum(w for (w, _) in edges)
print maxweight
print maxweight - total
