import pytest
from main import (
    Maze,
    read_puzzle_input,
)


@pytest.fixture
def puzzle_input():
    return read_puzzle_input("test_input.txt")


def test_maze_risk_of_shortest_path(puzzle_input):
    maze = Maze(puzzle_input)
    assert 40 == maze.risk_of_path_without_start(maze.find_shortest_path())


def test_maze_find_lowest_risk_cumsum(puzzle_input):
    maze = Maze(puzzle_input)
    assert 40 == maze.find_lowest_risk_cumsum()


# @pytest.fixture
# def fast_polymer():
#     return FastPolymer(*read_puzzle_input("test_input.txt"))
#
#
# def test_polymer_evolve(polymer):
#     assert "NNCB" == str(polymer)
#     assert "NCNBCHB" == str(polymer.evolve(1))
#     assert "NBCCNBBBCBHCB" == str(polymer.evolve(1))
#     assert "NBBBCNCCNBBNBNBBCHBHHBCHB" == str(polymer.evolve(1))
#     assert "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB" == str(polymer.evolve(1))
#
#
# @pytest.mark.parametrize(
#     "count, steps",
#     [
#         (1, 0),
#         (1, 1),
#         (5, 2),
#         (7, 3),
#         (1588, 10),
#     ],
# )
# def test_polymer_mc_minus_lc_element_count(polymer, count, steps):
#     polymer.evolve(steps)
#     assert count == polymer.mc_minus_lc_element_count()
#
#
# @pytest.mark.parametrize(
#     "count, steps",
#     [
#         (1, 0),
#         (1, 1),
#         (5, 2),
#         (7, 3),
#         (1588, 10),
#         (2188189693529, 40),
#     ],
# )
# def test_mc_minus_lc_element_count(fast_polymer, count, steps):
#     fast_polymer.evolve(steps)
#     assert count == fast_polymer.mc_minus_lc_element_count()