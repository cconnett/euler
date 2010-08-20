from collections import defaultdict

lens = [defaultdict(int)]
lens[0][''] = 1

def normtag(tag):
    return ''.join(sorted(set(tag)))

while len(lens) <= 40:
    nextlen = defaultdict(int)
    for next in '0123456789':
        curlen = lens[-1]
        for tag in curlen:
            num = curlen[tag]
            tagelts = list(tag)
            tagnums = [elt for elt in tagelts if '0'<=elt<='9']
            try:
                lastnum = [elt for elt in tagelts if 'A'<=elt<='J'][0]
                lastnum = ord(lastnum) - ord('A')
            except IndexError:
                lastnum = None
            if lastnum is not None and abs(int(next) - lastnum) != 1:
                continue
            nextlen[normtag(''.join(tagnums)+next+'ABCDEFGHIJ'[int(next)])] += num
    print nextlen
    lens.append(nextlen)
print len(lens)
them = list(lens[ell]['0123456789'+nonzero]
            for ell in range(len(lens))
            for nonzero in 'BCDEFGHIJ')
print sum(them)
