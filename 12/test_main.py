import pytest
from main import (
    read_puzzle_input,
    calc_n_paths,
)


@pytest.mark.parametrize(
    "test_input, n_paths",
    [
        ("small_test_input.txt", 10),
        ("medium_test_input.txt", 19),
        ("large_test_input.txt", 226),
    ],
)
def test_calc_n_paths_with_test_inputs(test_input, n_paths):
    puzzle_input = read_puzzle_input(test_input)
    assert n_paths == calc_n_paths(puzzle_input)
