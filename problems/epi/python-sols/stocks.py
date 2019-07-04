from typing import List

def maxProfit(prices: List[int]) -> int:
    if not prices or len(prices) == 1:
           return 0 
    currMax = 0; lp = prices[0]; hp = prices[0] 
    for n in prices:
        if n < lp:
            if hp - lp > currMax:
                currMax = hp - lp 
            lp = n 
            hp = n 
        elif n > hp:
            hp = n 
    return hp-lp if hp-lp > currMax else currMax

if __name__ == '__main__':
    ps0 = [7,1,5,3,6,4]
    ps1 = [7,6,4,3,1]

    assert maxProfit(ps0) == 5
    assert maxProfit(ps1) == 0
