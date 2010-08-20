def stu():
    a = zeros((40,40), 'f')
    for i in range(40):
        for j in range(40):
            #print j-i, {0:1,1:2,2:3,3:4,4:3,5:2,6:1}.get(j-i, 0)/16.0
            a[i][j] = {0:1,1:2,2:3,3:4,4:3,5:2,6:1}.get((j-i)%40, 0)/16.0
    return a
print >> file('monop.mat', 'w'), str(a.tolist()).replace('],','],\n')
