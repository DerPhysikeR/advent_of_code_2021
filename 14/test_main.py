import pytest
from main import (
    Polymer,
    read_puzzle_input,
)


@pytest.fixture
def polymer():
    return Polymer(*read_puzzle_input("test_input.txt"))


def test_polymer_evolve(polymer):
    assert "NNCB" == str(polymer)
    assert "NCNBCHB" == str(polymer.evolve(1))
    assert "NBCCNBBBCBHCB" == str(polymer.evolve(1))
    assert "NBBBCNCCNBBNBNBBCHBHHBCHB" == str(polymer.evolve(1))
    assert "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB" == str(polymer.evolve(1))


def test_mc_minus_lc_element_count(polymer):
    polymer.evolve(10)
    assert 1588 == polymer.mc_minus_lc_element_count()

