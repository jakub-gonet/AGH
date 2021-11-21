from testing import test


def knapsack_solver(data, max_capacity):
    def knapsack(i, remaining_capacity):
        value, weight = data[i]
        weight_diff = remaining_capacity - weight
        if remaining_capacity <= 0:
            return 0
        if i == 0:
            return value if weight_diff >= 0 else 0

        taken_value = knapsack(i - 1, weight_diff) + value if weight_diff >= 0 else 0
        not_taken_value = knapsack(i - 1, remaining_capacity)

        return max(taken_value, not_taken_value)

    return knapsack(len(data) - 1, max_capacity)


def knapsack_solver_memo(data, max_capacity):
    N = len(data)
    memo = [[None] * (max_capacity + 1) for _ in range(N + 1)]

    def knapsack(i, remaining_capacity):
        value, weight = data[i]
        weight_diff = remaining_capacity - weight
        if remaining_capacity <= 0:
            return 0
        if i == 0:
            return value if weight_diff >= 0 else 0

        taken_value = memo[i][weight_diff]
        if taken_value is None:
            taken_value = (
                knapsack(i - 1, weight_diff) + value if weight_diff >= 0 else 0
            )

        not_taken_value = memo[i][remaining_capacity]
        if not_taken_value is None:
            not_taken_value = knapsack(i - 1, remaining_capacity)

        return max(taken_value, not_taken_value)

    return knapsack(len(data) - 1, max_capacity)


def knapsack_solver_list(data, max_capacity):
    def knapsack(i, remaining_capacity, item_list):
        value, weight = data[i]
        weight_diff = remaining_capacity - weight
        if remaining_capacity <= 0:
            return (0, item_list)
        if i == 0:
            if weight_diff >= 0:
                item_list.append(value)
                return (value, item_list)
            else:
                return (0, item_list)

        taken_value, taken_list = knapsack(i - 1, weight_diff, item_list[:])
        taken_value = taken_value + value if weight_diff >= 0 else 0
        not_taken_value, not_taken_list = knapsack(
            i - 1, remaining_capacity, item_list[:]
        )
        if taken_value > not_taken_value:
            item_list = taken_list
            item_list.append(value)
        else:
            item_list = not_taken_list

        return (max(taken_value, not_taken_value), item_list)

    return knapsack(len(data) - 1, max_capacity, [])


if __name__ == "__main__":
    test_data = [
        ([[(7, 4), (3, 1), (2, 2), (10, 4), (4, 3), (1, 5), (7, 10), (2, 3)], 10], 20),
        (([(10, 10)], 10), 10),
        (([(7, 5), (3, 5)], 10), 10),
        (([(5, 5), (5, 4), (9, 10)], 10), 10),
        (([(1, 1), (1, 1), (1, 1)], 10), 3),
    ]

    test(knapsack_solver, test_data)
    test(knapsack_solver_memo, test_data)
    print(
        knapsack_solver_list(
            [(7, 4), (3, 1), (2, 2), (10, 4), (4, 3), (1, 5), (7, 10), (2, 3)], 10
        )
    )

