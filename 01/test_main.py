from main import (
    read_puzzle_input,
    count_depth_increases,
    count_windowed_depth_increases,
)


def test_count_depth_increases_with_test_input():
    assert 7 == count_depth_increases(read_puzzle_input("test_input.txt"))


def test_count_windowed_depth_increases_with_test_input():
    assert 5 == count_windowed_depth_increases(read_puzzle_input("test_input.txt"), 3)
