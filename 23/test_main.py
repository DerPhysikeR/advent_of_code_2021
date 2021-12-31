import pytest
from textwrap import dedent
from main import (
    State,
    sort_amphipods,
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

ADVANCED_TEST_INPUT = dedent(
    """\
    #############
    #...........#
    ###B#C#B#D###
      #D#C#B#A#
      #D#B#A#C#
      #A#D#C#A#
      #########
    """
)

ADVANCED_TEST_OUTPUT = dedent(
    """\
    #############
    #...........#
    ###A#B#C#D###
      #A#B#C#D#
      #A#B#C#D#
      #A#B#C#D#
      #########
    """
)


def test_state_from_map_and__str__():
    state = State.from_map(TEST_INPUT)
    assert str(state) == TEST_INPUT


@pytest.mark.parametrize(
    "room_idx, hallway_idx, distance",
    [
        (11, 1, 2),
        (26, 0, 12),
        (26, 10, 6),
        (16, 5, 3),
        (25, 2, 8),
    ],
)
def test_state_distance(room_idx, hallway_idx, distance):
    state = State.from_map(ADVANCED_TEST_OUTPUT)
    assert state.distance(room_idx, hallway_idx) == distance


@pytest.mark.parametrize(
    "state, idx, result",
    [
        (State("...........ABCDABCD"), ("A", 0), (3, State("A...........BCDABCD"))),
        (State("...........ABCDABCD"), ("D", 0), (9000, State("D..........ABC.ABCD"))),
        (State("...........ABCDABCD"), ("D", 10), (3000, State("..........DABC.ABCD"))),
        (State("...........ABCDABCD"), ("D", 7), (2000, State(".......D...ABC.ABCD"))),
        (State("A...........BCDABCD"), ("A", 1), (3, State("AA..........BCD.BCD"))),
    ],
)
def test_state_pop(state, idx, result):
    assert state.pop(*idx) == result


@pytest.mark.parametrize(
    "state, idx, error",
    [
        pytest.param(
            State("...........ABCDABCD"),
            ("A", -1),
            ValueError,
            id="invalid hallway idx",
        ),
        pytest.param(
            State("...........ABCDABCD"), ("A", 2), ValueError, id="invalid hallway idx"
        ),
        pytest.param(
            State("...........ABCDABCD"), ("E", 0), KeyError, id="invalid room idx"
        ),
        pytest.param(
            State("............BCD.BCD"), ("A", 0), ValueError, id="empty room"
        ),
        pytest.param(State(".B.........A.CDABCD"), ("A", 0), ValueError, id="blocked"),
    ],
)
def test_state_pop_raises(state, idx, error):
    with pytest.raises(error):
        state.pop(*idx)


# @pytest.mark.parametrize(
#     "state, idx, result",
#     [
#         (State("A...........BCDABCD"), ("A", 0), (3, State("...........ABCDABCD"))),
#         (State("A...........BCD.BCD"), ("A", 0), (4, State("............BCDABCD"))),
#         (State("..........A.BCD.BCD"), ("A", 10), (10, State("............BCDABCD"))),
#     ],
# )
# def test_state_push(state, idx, result):
#     assert state.push(*idx) == result


@pytest.mark.parametrize(
    "state, idx, error",
    [
        pytest.param(
            State("B..........ABCDABCD"), ("A", 0), ValueError, id="room allready full"
        ),
        pytest.param(State("BA.........ABCDABCD"), ("A", 0), ValueError, id="blocked"),
        pytest.param(
            State("B...........BCDABCD"), ("A", 0), ValueError, id="wrong amphipod"
        ),
        pytest.param(
            State("A..........ABC.ABCD"), ("D", 0), ValueError, id="wrong amphipod"
        ),
        pytest.param(
            State("AC.....B.BB....A.CDABCDADCD"),
            ("B", 7),
            ValueError,
            id="wrong amphipod",
        ),
        pytest.param(
            State("AC.....A...BCB.DCB.DBA.ADCD"),
            ("D", 7),
            ValueError,
            id="wrong amphipod",
        ),
        pytest.param(
            State("AC.....A...BCB.DCB.DBA.ADCD"),
            ("D", 9),
            ValueError,
            id="wrong amphipod",
        ),
    ],
)
def test_state_push_raises(state, idx, error):
    with pytest.raises(error):
        state.push(*idx)


@pytest.mark.parametrize("inp", [TEST_INPUT, ADVANCED_TEST_INPUT])
def test_state__str__(inp):
    assert inp == str(State.from_map(inp))


def test_sort_mini_amphipods():
    assert 8010 == sort_amphipods(State("...........DBCA"), State("...........ABCD"))


def test_sort_amphipods():
    assert 12521 == sort_amphipods(
        State.from_map(TEST_INPUT), State.from_map(TEST_OUTPUT)
    )


def test_sort_amphipods_with_more_amphipods():
    assert 44169 == sort_amphipods(
        State.from_map(ADVANCED_TEST_INPUT),
        State.from_map(ADVANCED_TEST_OUTPUT),
    )
