import pytest
from main import (
    read_puzzle_input,
    calc_corruptness_score,
    calc_incompleteness_score,
)


@pytest.fixture
def puzzle_input():
    return read_puzzle_input("test_input.txt")


def test_calc_corruptness_score(puzzle_input):
    assert 26397 == calc_corruptness_score(puzzle_input)


def test_calc_incompleteness_score(puzzle_input):
    assert 288957 == calc_incompleteness_score(puzzle_input)
