import random
import itertools

def dice():
    return random.randint(1,6) + random.randint(1,6)

def sim(n):
    sq = 0
    for i in range(n):
        sq += dice()
        sq %= 40
        if sq == 30:
            sq = 10
        elif sq in [2,17,33]:
            cc = random.randint(1,16)
            if cc == 1:
                sq = 0
            elif cc == 2:
                sq = 10
        elif sq in [7,22,36]:
            ch = random.randint(1,16)
            nextR = (sq + (5-sq%10)) % 40
            nextU = 28 if 12 < sq <= 28 else 12
            sq = {1:0, 2:10, 3:11, 4: 24, 5: 39, 6: 5, 7: nextR, 8:
                  nextR, 9: nextU, 10: (sq-3)%40, 11: sq, 12: sq, 13: sq,
                  14: sq, 15: sq, 16: sq}[ch]
    assert 0 <= sq < 40
    return sq

def p84():
    list(reversed(sorted([(len(list(group)),sq) for (sq, group) in itertools.groupby(sorted(sim(1000) for x in range(1000)))])))[:3]

def dicematrix():
    mat = zeros((40,40), Float)
    #freqs = [1,2,3,4,5,6,5,4,3,2,1]
    freqs = [1,2,3,4,3,2,1]
    for start in range(len(mat)):
        for (offset, freq) in zip(range(2,2+len(freqs)+1), freqs):
            mat[start][(start+offset)%40] = float(freq) / sum(freqs)

    #doubles = array([5./6,1./6,0,5./6,0,1./6,1,0,0])
    doubles = array([3./4, 1./4,  0.0,
                     3./4,    0, 1./4,
                        1,    0,    0])
    doubles.shape = (3,3)
    doubleslongrun = matrixpower(doubles,10000)
    
    deltas = zeros((40,40), Float)
    for start in range(len(mat)):
        for finish in range(len(mat[start])):
            deltas[start][finish] -= mat[start][finish] * doubleslongrun[0][2]/max(freqs)
            deltas[start][10] += mat[start][finish] * doubleslongrun[0][2]/max(freqs)
    mat += deltas

    deltas = zeros((40,40), Float)
    for start in range(len(mat)):
        for finish in range(len(mat[start])):
            if finish in [30]:
                deltas[start][finish] -= mat[start][finish]
                deltas[start][10] += mat[start][finish]
            if finish in [2,17,33]:
                deltas[start][finish] -= mat[start][finish] * (1/8.0)
                deltas[start][0] += mat[start][finish] * (1/16.0)
                deltas[start][10] += mat[start][finish] * (1/16.0)
            if finish in [7,22,36]:
                nextR = (finish + (5-finish%10)) % 40
                nextU = 28 if 12 < finish <= 28 else 12
                
                deltas[start][finish] -= mat[start][finish] * (10/16.0)
                deltas[start][0] += mat[start][finish] * (1/16.0)
                deltas[start][10] += mat[start][finish] * (1/16.0)
                deltas[start][11] += mat[start][finish] * (1/16.0)
                deltas[start][24] += mat[start][finish] * (1/16.0)
                deltas[start][39] += mat[start][finish] * (1/16.0)
                deltas[start][5] += mat[start][finish] * (1/16.0)
                # NB: nextR --- probability 2/16
                deltas[start][nextR] += mat[start][finish] * (2/16.0)
                deltas[start][nextU] += mat[start][finish] * (1/16.0)
                deltas[start][(finish-3)%40] += mat[start][finish] * (1/16.0)
    return mat + deltas

def matrixpower(matrix, n):
    assert n >= 1
    
    maxpower = 1
    while maxpower < n:
        maxpower <<= 1

    powers = [matrix]
    curpower = 1
    while curpower < maxpower:
        curpower <<= 1
        powers.append(matrixmultiply(powers[-1], powers[-1]))
    
    result = identity(matrix.shape[0])
    bit = 1
    while (1<<bit) <= maxpower:
        if n & (1<<bit):
            result = matrixmultiply(result, powers[bit])
        bit += 1
    return result

def top(dm, n):
    for (prob, sq) in list(sorted(zip(
        matrixpower(dm, n)[0], range(0,40)), reverse = True)):
        print '%4.2f%% %02d' % (prob*100, sq)
