import pytest
from main import (
    Point,
    Line,
    read_puzzle_input,
    check_if_orthogonal,
    count_overlapping_points,
)


def test_read_puzzle_input():
    assert read_puzzle_input("test_input.txt")[0] == Line(Point(0, 9), Point(5, 9))


@pytest.mark.parametrize(
    "line, is_orthogonal",
    [
        (Line(Point(0, 0), Point(1, 0)), True),
        (Line(Point(0, 0), Point(1, 1)), False),
        (Line(Point(0, 0), Point(0, 1)), True),
    ],
)
def test_check_if_orthogonal(line: Line, is_orthogonal: bool):
    assert is_orthogonal == check_if_orthogonal(line)


@pytest.fixture
def lines():
    return read_puzzle_input("test_input.txt")


def test_count_overlapping_points_with_orthogonal_lines_only(lines):
    orthogonal_lines = [line for line in lines if check_if_orthogonal(line)]
    assert 5 == count_overlapping_points(orthogonal_lines)


def test_count_overlapping_points_with_all_lines(lines):
    assert 12 == count_overlapping_points(lines)
