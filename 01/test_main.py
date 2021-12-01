from main import read_puzzle_input, count_depth_increases


def test_count_depth_increases_with_test_input():
    assert 7 == count_depth_increases(read_puzzle_input("test_input.txt"))
