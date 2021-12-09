import pytest
from main import (
    Point,
    HeightMap,
    read_puzzle_input,
)


@pytest.fixture
def heightmap():
    return read_puzzle_input("test_input.txt")


def test_find_low_points(heightmap):
    assert [
        Point(0, 1),
        Point(0, 9),
        Point(2, 2),
        Point(4, 6),
    ] == heightmap.find_low_points()


def test_heightmap_width(heightmap):
    assert 10 == heightmap.width


def test_heightmap_height(heightmap):
    assert 5 == heightmap.height


def test_find_low_points_in_top_left_corner():
    heightmap = HeightMap([[0, 1], [1, 1]])
    assert [Point(0, 0)] == heightmap.find_low_points()


def test_point_neighbors():
    point = Point(0, 0)
    assert [Point(0, 1), Point(-1, 0), Point(0, -1), Point(1, 0)] == list(
        point.neighbors()
    )


def test_heightmap_neighbors_in_corner(heightmap):
    assert [Point(0, 1), Point(1, 0)] == list(heightmap.neighbors(Point(0, 0)))


def test_heightmap_neighbors_on_side(heightmap):
    assert [Point(0, 2), Point(0, 0), Point(1, 1)] == list(
        heightmap.neighbors(Point(0, 1))
    )


def test_heightmap_neighbors_in_the_middle(heightmap):
    assert [Point(1, 2), Point(0, 1), Point(1, 0), Point(2, 1)] == list(
        heightmap.neighbors(Point(1, 1))
    )


@pytest.mark.parametrize(
    "point, basin_size",
    [
        (Point(0, 1), 3),
        (Point(0, 9), 9),
        (Point(2, 2), 14),
        (Point(4, 6), 9),
    ],
)
def test_heightmap_find_basin_around(heightmap, point, basin_size):
    assert basin_size == len(heightmap.find_basin_around(point))


def test_product_of_3_largest_basin_sizes(heightmap):
    assert 1134 == heightmap.product_of_3_largest_basin_sizes()
