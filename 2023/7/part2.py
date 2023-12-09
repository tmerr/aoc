from collections import Counter


def strength(hand):
    # Consider this up front to remove edge cases.
    if hand == 'JJJJJ':
        return (5, -1, (0,)*5)
    counts = Counter(hand)
    jokerCount = counts['J']
    del counts['J']
    maxCount = max(counts.values()) + jokerCount
    cardStrengths = tuple(map('J23456789TQKA'.index, hand))
    return (maxCount, -len(counts), cardStrengths)


def run():
    hands = []
    bids = {}
    strengths = {}
    with open('input.txt') as f:
        for line in f:
            toks = iter(line.strip().split())
            hand = next(toks)
            hands.append(hand)
            bid = int(next(toks))
            bids[hand] = bid
            strengths[hand] = strength(hand)
    winnings = 0
    for i, card in enumerate(sorted(hands, key=lambda h: strengths[h])):
        winnings += (i+1) * bids[card]
    print(winnings)


if __name__ == '__main__':
    run()