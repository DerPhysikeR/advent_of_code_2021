import pytest
from main import (
    read_puzzle_input,
    School,
)


def test_read_puzzle_input():
    assert [3, 4, 3, 1, 2] == read_puzzle_input("test_input.txt")


@pytest.fixture
def fishies():
    return School(read_puzzle_input("test_input.txt"))


@pytest.mark.parametrize("n_days, n_of_fishies", [(18, 26), (80, 5934)])
def test_school_evolve_n_days(fishies, n_days, n_of_fishies):
    for _ in range(n_days):
        fishies.evolve()
        print(fishies)
    assert n_of_fishies == len(fishies)
