import pytest
from textwrap import dedent
from main import (
    State,
    sort_amphipods,
    # find_path,
)

TEST_INPUT = dedent(
    """\
    #############
    #...........#
    ###B#C#B#D###
      #A#D#C#A#
      #########
    """
)

TEST_OUTPUT = dedent(
    """\
    #############
    #...........#
    ###A#B#C#D###
      #A#B#C#D#
      #########
    """
)


def test_state_from_map_and__str__():
    state = State.from_map(TEST_INPUT)
    assert str(state) == TEST_INPUT


def test_state_letters():
    assert (
        (11, "B"),
        (12, "C"),
        (13, "B"),
        (14, "D"),
        (15, "A"),
        (16, "D"),
        (17, "C"),
        (18, "A"),
    ) == tuple(State.from_map(TEST_INPUT).letters())


def test_sort_amphipods():
    assert 12521 == sort_amphipods(TEST_INPUT)
