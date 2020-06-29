from testing import test


def subset_sum(A, T):
    def find(current_sum, i):
        if current_sum == T:
            return A != []  # taking care of case when A=[], T=0
        if i == -1:
            return False
        return find(current_sum + A[i], i - 1) or find(current_sum, i - 1)

    return find(0, len(A) - 1)


test(
    subset_sum,
    [
        (([1, 3, 5, 6], 10), True),
        (([1, 0, 1], 10), False),
        (([], 1), False),
        (([], 0), False),
        (([-5, 4], -1), True),
    ],
)


def longest_common_subsequence(A, B):
    N = len(A)

    def find_longest(a, b, length):
        if a == -1 or b == -1:
            return length
        if A[a] == B[b]:
            return find_longest(a - 1, b - 1, length + 1)

        return max(find_longest(a - 1, b, length), find_longest(a, b - 1, length))

    return find_longest(N - 1, N - 1, 0)


test(
    longest_common_subsequence,
    [
        (([1, 2, 3, 4, 5, 6], [1, 4, 6, 0, 0, 0]), 3),
        (([1, 1, 1, 1, 1, 1, 1, 1, 1], [0, 0, 0, 0, 0, 0, 0, 0, 0]), 0),
        (([1, 2, 3, 4, 5], [5, 4, 3, 2, 1]), 1),
    ],
)


def matrix_multiplication_optimization(matrices_dim):
    """ matrix A[i] is A[i-1] x A[i] """

    def f(start, end):
        if start == end:
            return 0

        return min(
            (
                f(i + 1, end)
                + f(start, i)
                + matrices_dim[start - 1] * matrices_dim[i] * matrices_dim[end]
                for i in range(start, end)
            )
            or [0]
        )

    return f(1, len(matrices_dim) - 1)


test(matrix_multiplication_optimization, [(([10, 20, 30, 40, 30],), 30000)])


def cash_change_opt(nom, T):
    def f(T):
        if T == 0:
            return 0
        if T < 0:
            return None
        min_change = None
        for n in nom:
            change = f(T - n)
            if change is not None:
                if min_change is None or change < min_change:
                    min_change = change + 1
        return min_change

    return f(T)


test(cash_change_opt, [(([1, 5, 8], 15), 3), (([2], 7), None)])


def chessboard_cheapest_route(A):
    N = len(A) - 1

    def f(x, y):
        if x == N and y == N:
            return A[N][N]

        if x > N or y > N:
            return None

        horizontal = f(x + 1, y)
        vertical = f(x, y + 1)
        cheapest = None

        if horizontal is None:
            cheapest = vertical
        elif vertical is None:
            cheapest = horizontal
        else:
            cheapest = min(horizontal, vertical)

        if cheapest is not None:
            cheapest += A[y][x]
        return cheapest

    return f(0, 0)


test(
    chessboard_cheapest_route,
    [
        (([[1, 1, 1], [2, 2, 1], [2, 2, 1]],), 5),
        (([[0, 0, 0], [2, 2, 0], [2, 2, 0]],), 0),
        (([[0, 0, -1], [2, 2, 0], [2, 2, 0]],), -1),
    ],
)

