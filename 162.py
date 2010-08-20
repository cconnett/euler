from collections import defaultdict

lens = [defaultdict(int)]
lens[0][''] = 1

def normtag(tag):
    return ''.join(sorted(set(tag)))

while len(lens) <= 16:
    nextlen = defaultdict(int)
    for next in '0123456789ABCDEF':
        curlen = lens[-1]
        for tag in curlen:
            num = curlen[tag]
            nexttag = tag
            if next == '0':
                nexttag += '0L'
            else:
                if 'L' in tag:
                    tagletters = list(tag)
                    tagletters.remove('L')
                    nexttag = ''.join(tagletters)
            if next in '1A':
                nexttag += next
            nextlen[normtag(nexttag)] += num
    print nextlen
    lens.append(nextlen)
print len(lens)
print list('%X' % lens[ell]['01A'] for ell in range(len(lens)))
print '%X' % sum(lens[ell]['01A'] for ell in range(len(lens)))
