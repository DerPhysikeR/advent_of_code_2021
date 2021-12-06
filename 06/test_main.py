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


@pytest.mark.parametrize(
    "n_days, n_fishies", [(18, 26), (80, 5934), (256, 26984457539)]
)
def test_school_evolve_n_days(fishies, n_days, n_fishies):
    assert n_fishies == len(fishies.evolve(n_days))
