import pytest
from main import (
    Die,
    Player,
    Game,
    find_points_loser_times_dice_rolls,
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
