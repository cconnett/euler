from Numeric import *

cur = zeros((3,31))

cur[0,0] = 1
curlen = 0

print curlen
print cur
while curlen < 30:
    for totalOs in range(cur.shape[1] - 1, -1, -1):
        cur[2,totalOs] = cur[1,totalOs]
        cur[1,totalOs] = cur[0,totalOs]
        try:
            cur[0,totalOs] = sum(cur[:,totalOs-1])
        except:
            cur[0,totalOs] = 0
    curlen += 1
    print curlen
    print cur

print cur[0,:]
ans = 0
for totalOs in range(cur.shape[1]):
    ans += sum(cur[:,totalOs]) * (totalOs+1)
print ans
