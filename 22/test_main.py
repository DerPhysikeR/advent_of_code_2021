import pytest
from main import (
    Cube,
    Cuboid,
    Instruction,
    read_puzzle_input,
    INIT_REGION,
    reboot,
    on_cubes_in_init_region,
    count_cubes,
    count_after_instructions,
)


@pytest.fixture
def instructions():
    return read_puzzle_input("test_input.txt")


@pytest.fixture
def more_instructions():
    return read_puzzle_input("large_test_input.txt")


def test_read_puzzle_input(instructions):
    assert instructions[0] == Instruction(
        True, Cuboid(Cube(-20, -36, -47), Cube(26, 17, 7))
    )
    assert instructions[-1] == Instruction(
        True, Cuboid(Cube(967, 45373, 27513), Cube(23432, 81175, 53682))
    )


def test_reboot(instructions):
    assert sum(1 if c in INIT_REGION else 0 for c in reboot(instructions)) == 590784


def test_on_cubes_int_init_region(instructions):
    assert on_cubes_in_init_region(instructions) == 590784


@pytest.mark.parametrize(
    "cuboid, size",
    [
        (Cuboid(Cube(0, 0, 0), Cube(0, 0, 0)), 1),
        (Cuboid(Cube(0, 0, 0), Cube(1, 1, 1)), 8),
        (Cuboid(Cube(0, 0, 0), Cube(2, 1, 0)), 6),
    ],
)
def test_cuboid_size(cuboid, size):
    assert cuboid.size == size


def test_count_cubes_with_non_overlapping_cuboids():
    cuboids = [
        Cuboid(Cube(1, 1, 0), Cube(10, 10, 0)),
        Cuboid(Cube(21, 21, 0), Cube(30, 30, 0)),
    ]
    assert count_cubes(cuboids) == 200


def test_count_cubes_with_single_overlapping_cuboids():
    cuboids = [
        Cuboid(Cube(1, 0, 0), Cube(10, 0, 0)),
        Cuboid(Cube(6, 0, 0), Cube(15, 0, 0)),
    ]
    assert count_cubes(cuboids) == 15


def test_count_cubes_with_identical_cuboids():
    cuboids = [
        Cuboid(Cube(1, 0, 0), Cube(10, 0, 0)),
        Cuboid(Cube(1, 0, 0), Cube(10, 0, 0)),
    ]
    assert count_cubes(cuboids) == 10


def test_count_cubes_after_instructions(more_instructions):
    assert count_after_instructions(more_instructions) == 2758514936282235
