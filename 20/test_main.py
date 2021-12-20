import pytest
from main import (
    Point,
    read_puzzle_input,
    count_light_pixels_after_enhancements,
)

IMAGE = "#..#.\n#....\n##..#\n..#..\n..###"
ENHANCED_IMAGE = ".##.##.\n#..#.#.\n##.#..#\n####..#\n.#..##.\n..##..#\n...#.#."
TWICE_ENHANCED_IMAGE = ".......#.\n.#..#.#..\n#.#...###\n#...##.#.\n#.....#.#\n.#.#####.\n..#.#####\n...##.##.\n....###.."


def test_point_neighbors():
    point = Point(5, 10)
    it = iter(point.neighbors())
    assert next(it) == Point(4, 9)
    assert next(it) == Point(4, 10)
    assert next(it) == Point(4, 11)
    assert next(it) == Point(5, 9)
    assert next(it) == Point(5, 10)
    assert next(it) == Point(5, 11)
    assert next(it) == Point(6, 9)
    assert next(it) == Point(6, 10)
    assert next(it) == Point(6, 11)
    with pytest.raises(StopIteration):
        next(it)


@pytest.fixture
def puzzle_input():
    return read_puzzle_input("test_input.txt")


def test_count_light_pixels_after_enhancements(puzzle_input):
    assert 35 == count_light_pixels_after_enhancements(*puzzle_input, 2)


def test_image__str__(puzzle_input):
    image = puzzle_input[0]
    assert str(image) == IMAGE


def test_image_enhance(puzzle_input):
    image, algo = puzzle_input
    assert str(image := image.enhance(algo)) == ENHANCED_IMAGE
    assert str(image := image.enhance(algo)) == TWICE_ENHANCED_IMAGE
