import pytest
from main import (
    SFN,
    sum_sfn,
    read_puzzle_input,
)


@pytest.fixture
def puzzle_input():
    return read_puzzle_input("test_input.txt")


@pytest.mark.parametrize(
    "numbers, result",
    [
        ([[1, 2], [3, 4]], SFN([[1, 2], [3, 4]])),
        ([[1, 1], [2, 2], [3, 3], [4, 4]], SFN([[[[1, 1], [2, 2]], [3, 3]], [4, 4]])),
        (
            [[1, 1], [2, 2], [3, 3], [4, 4], [5, 5]],
            SFN([[[[3, 0], [5, 3]], [4, 4]], [5, 5]]),
        ),
        (
            [[1, 1], [2, 2], [3, 3], [4, 4], [5, 5], [6, 6]],
            SFN([[[[5, 0], [7, 4]], [5, 5]], [6, 6]]),
        ),
    ],
)
def test_sum_sfn(numbers, result):
    assert result == sum_sfn(numbers)


def test_sum_sfn_with_test_input(puzzle_input):
    assert SFN(
        [[[[8, 7], [7, 7]], [[8, 6], [7, 7]]], [[[0, 7], [6, 6]], [8, 7]]]
    ) == sum_sfn(puzzle_input)


@pytest.mark.parametrize(
    "num, rep",
    [
        ([1, 2], "SFN([1, 2])"),
        ([[5, [7, 8]], 2], "SFN([[5, [7, 8]], 2])"),
        ([1, [3, 4]], "SFN([1, [3, 4]])"),
        ([1, [[7, 8], 4]], "SFN([1, [[7, 8], 4]])"),
    ],
)
def test_sfn_repr(num, rep):
    assert rep == repr(SFN(num))


@pytest.mark.parametrize(
    "left, right, result",
    [
        ([1, 2], [3, 4], [[1, 2], [3, 4]]),
        ([1, 2], [[3, 4], 5], [[1, 2], [[3, 4], 5]]),
        (
            [[[[4, 3], 4], 4], [7, [[8, 4], 9]]],
            [1, 1],
            [[[[0, 7], 4], [[7, 8], [6, 0]]], [8, 1]],
        ),
        (
            [[[0, [4, 5]], [0, 0]], [[[4, 5], [2, 6]], [9, 5]]],
            [7, [[[3, 7], [4, 3]], [[6, 3], [8, 8]]]],
            [[[[4, 0], [5, 4]], [[7, 7], [6, 0]]], [[8, [7, 7]], [[7, 9], [5, 0]]]],
        ),
        (
            [[[[4, 0], [5, 4]], [[7, 7], [6, 0]]], [[8, [7, 7]], [[7, 9], [5, 0]]]],
            [[2, [[0, 8], [3, 4]]], [[[6, 7], 1], [7, [1, 6]]]],
            [
                [[[6, 7], [6, 7]], [[7, 7], [0, 7]]],
                [[[8, 7], [7, 7]], [[8, 8], [8, 0]]],
            ],
        ),
    ],
)
def test_sfn_add(left, right, result):
    assert SFN(result) == SFN(left) + SFN(right)


@pytest.mark.parametrize(
    "tree, reduced_tree",
    [
        ([10, 2], [[5, 5], 2]),
        ([[[[[5, 6], 1], 2], 3], 4], [[[[0, 7], 2], 3], 4]),
        (
            [0, [14, [[[3, 7], [4, 3]], [[6, 3], [8, 8]]]]],
            [0, [[8, [7, 7]], [[7, 9], [5, 0]]]],
        ),
    ],
)
def test_sfn_reduce(tree, reduced_tree):
    assert reduced_tree == SFN(tree).to_list()


@pytest.mark.parametrize(
    "tree, magnitude",
    [
        ([[1, 2], [[3, 4], 5]], 143),
        ([[[[0, 7], 4], [[7, 8], [6, 0]]], [8, 1]], 1384),
        ([[[[1, 1], [2, 2]], [3, 3]], [4, 4]], 445),
        ([[[[3, 0], [5, 3]], [4, 4]], [5, 5]], 791),
        ([[[[5, 0], [7, 4]], [5, 5]], [6, 6]], 1137),
        ([[[[8, 7], [7, 7]], [[8, 6], [7, 7]]], [[[0, 7], [6, 6]], [8, 7]]], 3488),
    ],
)
def test_sfn_magnitude(tree, magnitude):
    assert magnitude == SFN(tree).magnitude()
