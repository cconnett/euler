import itertools    

def ranks(hand):
    return [card[0] for card in hand]
def suits(hand):
    return [card[1] for card in hand]

def group(hand):
    hand.sort()
    groups = []
    for i in range(4,0,-1):
        for rank in sorted(set(ranks(hand)), reverse = True):
            if ranks(hand).count(rank) == i:
                groups.append([card for card in hand if card[0] == rank])
    return groups

def flush(hand):
    return len(set(suits(hand))) == 1
def straight(hand):
    return sorted(ranks(hand)) == range(min(ranks(hand)), max(ranks(hand))+1)

def rankhand(hand):
    if flush(hand) and straight(hand):
        return (9, max(ranks(hand)))
    if len(group(hand)[0]) == 4:
        return (8, (ranks(group(hand)[0])[0], ranks(list(itertools.chain(*group(hand)[1:])))))
    if len(group(hand)[0]) == 3 and len(group(hand)[1]) == 2:
        return (7, (ranks(group(hand)[0]), ranks(group(hand)[1])))
    if flush(hand):
        return (6, sorted(ranks(hand), reverse = True))
    if straight(hand):
        return (5, sorted(ranks(hand), reverse = True))
    if len(group(hand)[0]) == 3:
        return (4, (ranks(group(hand)[0])[0], ranks(list(itertools.chain(*group(hand)[1:])))))
    if len(group(hand)[0]) == 2 and len(group(hand)[1]) == 2:
        return (3, (ranks(group(hand)[0]),
                    ranks(group(hand)[1]),
                    ranks(group(hand)[2])))
    if len(group(hand)[0]) == 2:
        return (2, (ranks(group(hand)[0])[0], ranks(list(itertools.chain(*group(hand)[1:])))))
    return (1, sorted(ranks(hand), reverse = True))

def readcard(str):
    try:
        rank = int(str[0])
    except ValueError:
        rank = {'T':10,'J':11,'Q':12,'K':13,'A':14}[str[0]]
    return (rank, str[1])

if __name__ == "__main__":
    hands = [[readcard(card) for card in line.split()]
             for line in file('poker.txt').readlines()]
    hands = [(hand[:5], hand[5:]) for hand in hands]
    print len([1 for (hand1, hand2) in hands if rankhand(hand1) > rankhand(hand2)])

    
