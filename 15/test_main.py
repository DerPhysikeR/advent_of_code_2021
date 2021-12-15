import pytest
from main import (
    Dijkstra,
    read_puzzle_input,
)


@pytest.fixture
def puzzle_input():
    return read_puzzle_input("test_input.txt")


def test_dijkstra_find_lowest_risk(puzzle_input):
    maze = Dijkstra(puzzle_input)
    assert 40 == maze.find_lowest_risk()
