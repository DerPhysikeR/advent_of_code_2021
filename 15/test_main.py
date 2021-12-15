import pytest
from main import (
    CumSum,
    Dijkstra,
    read_puzzle_input,
    expand_puzzle_input,
)


@pytest.fixture
def puzzle_input():
    return read_puzzle_input("test_input.txt")


@pytest.fixture
def large_puzzle_input():
    puzzle_input = read_puzzle_input("test_input.txt")
    return expand_puzzle_input(puzzle_input)


def test_cumsum_find_lowest_risk(puzzle_input):
    maze = CumSum(puzzle_input)
    assert 40 == maze.find_lowest_risk()


def test_dijkstra_find_lowest_risk(puzzle_input):
    maze = Dijkstra(puzzle_input)
    assert 40 == maze.find_lowest_risk()


def test_dijkstra_find_lowest_risk_for_larger_input(large_puzzle_input):
    maze = Dijkstra(large_puzzle_input)
    assert 315 == maze.find_lowest_risk()
