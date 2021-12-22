import pytest
from main import (
    Cube,
    Cuboid,
    Instruction,
    read_puzzle_input,
    INIT_REGION,
    reboot,
    on_cubes_int_init_region,
    # Die,
    # Player,
    # Game,
    # find_points_loser_times_dice_rolls,
    # calc_new_position,
)


@pytest.fixture
def instructions():
    return read_puzzle_input("test_input.txt")


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
    assert on_cubes_int_init_region(instructions) == 590784
