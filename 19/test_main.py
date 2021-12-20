from parse import parse
import pytest
from main import (
    Point,
    Cube,
    read_puzzle_input,
    assemble_map,
    max_manhatten_distance,
)


@pytest.fixture
def puzzle_input():
    return read_puzzle_input("test_input.txt")


@pytest.fixture
def puzzle_output():
    with open("test_output.txt") as stream:
        return [
            Point(*parse("{:d},{:d},{:d}", line))
            for line in stream.read().strip().split("\n")
        ]


def test_cube_orientations():
    cube = Cube([Point(1000, 1000, 1000), Point(1, 0, 0), Point(0, 2, 0)])
    sets = [set(c.beacons) for c in cube.orientations()]
    assert len(sets) == 24
    for i, s in enumerate(sets):
        assert s not in sets[:i]
        assert s not in sets[i + 1 :]


def test_assemble_map(puzzle_input, puzzle_output):
    map = assemble_map([Cube(beacons=c) for c in puzzle_input])
    assert 12 == len(set(map[0].beacons).intersection(set(map[1].beacons)))
    beacons = set().union(*[set(c.beacons) for c in map])
    assert beacons == set(puzzle_output)


def test_max_manhatten_distances(puzzle_input):
    map = assemble_map([Cube(beacons=c) for c in puzzle_input])
    assert 3621 == max_manhatten_distance([c.scanner for c in map])
