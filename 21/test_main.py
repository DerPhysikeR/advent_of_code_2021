import pytest
from main import (
    Die,
    Player,
    Game,
    find_points_loser_times_dice_rolls,
    calc_new_position,
    find_winning_universe_counts,
)


def test_die_sequence():
    die = Die()
    for _ in range(100):
        die.roll()
    assert 1 == die.roll()


def test_game_play_until_one_player_reaches_1000():
    game = Game(Die(), Player(4), Player(8))
    game.play_until_one_player_reaches_1000()
    assert 739785 == game.loser.points * game.die.n_rolls


def test_find_points_loser_times_dice_rolls():
    assert 739785 == find_points_loser_times_dice_rolls((4, 8))


@pytest.mark.parametrize(
    "position, throw, new_position",
    [
        (9, 1, 10),
        (8, 2, 10),
        (10, 2, 2),
        (9, 3, 2),
    ],
)
def test_calc_new_position(position, throw, new_position):
    assert new_position == calc_new_position(position, throw)


def test_find_winning_universe_counts():
    p1_win_count, p2_win_count = find_winning_universe_counts([4, 8])
    assert p1_win_count == 444356092776315
    assert p2_win_count == 341960390180808
