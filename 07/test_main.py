import pytest
from main import (
    read_puzzle_input,
    calc_fuel_cost,
    find_min_fuel_pos,
    calc_fuel_cost_2,
)


@pytest.fixture
def crabs():
    return read_puzzle_input("test_input.txt")


def test_read_puzzle_input(crabs):
    assert [16, 1, 2, 0, 4, 2, 7, 1, 2, 14] == crabs


@pytest.mark.parametrize(
    "alignment_pos, fuel_cost",
    [
        (2, 37),
        (1, 41),
        (3, 39),
        (10, 71),
    ],
)
def test_calc_fuel_cost(crabs, alignment_pos, fuel_cost):
    assert fuel_cost == calc_fuel_cost(crabs, alignment_pos)


@pytest.mark.parametrize(
    "crabs, alignment_pos, fuel_cost",
    [
        ([16], 5, 66),
        ([1], 5, 10),
        ([5], 5, 0),
        ([-1, 1], 0, 2),
        ([-1, 2], 0, 4),
    ],
)
def test_calc_fuel_cost_2(crabs, alignment_pos, fuel_cost):
    assert fuel_cost == calc_fuel_cost_2(crabs, alignment_pos)


def test_find_min_fuel_pos(crabs):
    assert (37, 2) == find_min_fuel_pos(crabs)


def test_find_min_fuel_pos_2(crabs):
    assert (168, 5) == find_min_fuel_pos(crabs, calc_fuel_cost_2)
