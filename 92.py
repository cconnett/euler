ea89 = [1] * 10000000
def sqd(n): return sum(int(x)**2 for x in list(str(n)))
def f(n):
    if ea89[n] == 2: return 2
    if ea89[n] == 0: return 0
    if ea89[n] == 1:
        ea89[n] = f(sqd(n))
        return ea89[n]

ea89[89] = 2
ea89[1] = 0

