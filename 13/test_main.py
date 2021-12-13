import pytest
from main import (
    Point,
    Fold,
    Paper,
    read_puzzle_input,
)


@pytest.fixture
def puzzle_input():
    return read_puzzle_input("test_input.txt")


def test_read_puzzle_input(puzzle_input):
    points, folds = puzzle_input
    assert 18 == len(points)
    assert Point(6, 10) in points
    assert Point(9, 0) in points
    assert [Fold("y", 7), Fold("x", 5)] == folds


def test_paper_fold(puzzle_input):
    points, folds = puzzle_input
    paper = Paper(points)
    assert 18 == len(paper)
    assert 17 == len(paper.fold(folds[0]))
    assert 16 == len(paper.folds(folds))
