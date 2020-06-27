from testing import test


def fibbonacci(n):
    """Defined for n >= 0"""
    if n <= 1:
        return 1
    return fibbonacci(n - 1) + fibbonacci(n - 2)


def fibbonacci_memo(n):
    """Defined for n >= 0, uses more memory"""
    memo = [1] * (n + 1)
    for i in range(2, n + 1):
        memo[i] = memo[i - 1] + memo[i - 2]
    return memo[n]


def fibbonacci_better_memo(n):
    """Defined for n >= 0, uses const memory"""

    prev_1 = 1
    prev_2 = 1
    current = 1
    for _ in range(2, n + 1):
        current = prev_1 + prev_2
        prev_1 = prev_2
        prev_2 = current
    return current


test(fibbonacci, [([0], 1), ([1], 1), ([2], 2), ([3], 3), ([4], 5), ([5], 8)])
test(fibbonacci_memo, [([0], 1), ([1], 1), ([2], 2), ([3], 3), ([4], 5), ([5], 8)])
test(
    fibbonacci_better_memo, [([0], 1), ([1], 1), ([2], 2), ([3], 3), ([4], 5), ([5], 8)]
)

