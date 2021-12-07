import pytest
from main import (
    read_puzzle_input,
    calc_fuel_cost,
    find_minimal_fuel_cost_and_position,
)


@pytest.fixture
def crabs():
    return read_puzzle_input("test_input.txt")


def test_read_puzzle_input(crabs):
    assert [16, 1, 2, 0, 4, 2, 7, 1, 2, 14] == crabs


@pytest.mark.parametrize(
    "alignment_position, fuel_cost",
    [
        (2, 37),
        (1, 41),
        (3, 39),
        (10, 71),
    ],
)
def test_fuel_cost(crabs, alignment_position, fuel_cost):
    assert fuel_cost == calc_fuel_cost(crabs, alignment_position)


def test_find_minimal_fuel_cost_and_position(crabs):
    assert (37, 2) == find_minimal_fuel_cost_and_position(crabs)
