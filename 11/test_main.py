import pytest
from main import (
    read_puzzle_input,
    Grid,
)


@pytest.fixture
def puzzle_input():
    return read_puzzle_input("test_input.txt")


@pytest.mark.parametrize(
    "n_steps, n_flashes",
    [
        (10, 204),
        (100, 1656),
    ],
)
def test_grid_evolve_n_flashes(puzzle_input, n_steps, n_flashes):
    grid = Grid(puzzle_input)
    assert n_flashes == grid.evolve(n_steps)
