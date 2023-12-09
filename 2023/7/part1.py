from collections import Counter


def strength(hand):
    counts = Counter(hand)
    maxCount = max(counts.values())
    cardStrengths = tuple(map('23456789TJQKA'.index, hand))
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