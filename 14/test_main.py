import pytest
from main import (
    Polymer,
    FastPolymer,
    read_puzzle_input,
)


@pytest.fixture
def polymer():
    return Polymer(*read_puzzle_input("test_input.txt"))


@pytest.fixture
def fast_polymer():
    return FastPolymer(*read_puzzle_input("test_input.txt"))


def test_polymer_evolve(polymer):
    assert "NNCB" == str(polymer)
    assert "NCNBCHB" == str(polymer.evolve(1))
    assert "NBCCNBBBCBHCB" == str(polymer.evolve(1))
    assert "NBBBCNCCNBBNBNBBCHBHHBCHB" == str(polymer.evolve(1))
    assert "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB" == str(polymer.evolve(1))


@pytest.mark.parametrize(
    "count, steps",
    [
        (1, 0),
        (1, 1),
        (5, 2),
        (7, 3),
        (1588, 10),
    ],
)
def test_polymer_mc_minus_lc_element_count(polymer, count, steps):
    polymer.evolve(steps)
    assert count == polymer.mc_minus_lc_element_count()


@pytest.mark.parametrize(
    "count, steps",
    [
        (1, 0),
        (1, 1),
        (5, 2),
        (7, 3),
        (1588, 10),
        (2188189693529, 40),
    ],
)
def test_mc_minus_lc_element_count(fast_polymer, count, steps):
    fast_polymer.evolve(steps)
    assert count == fast_polymer.mc_minus_lc_element_count()
