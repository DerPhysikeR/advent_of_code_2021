from __future__ import annotations
from types import GenericAlias
from typing import Iterator, NamedTuple, TypeVar
from enum import Enum
from itertools import cycle, product
from operator import attrgetter


class Die:
    def __init__(self):
        self.n_rolls = 0
        self.sequence = cycle(range(1, 100 + 1))

    def roll(self):
        self.n_rolls += 1
        return next(self.sequence)


class Player:
    def __init__(self, position, points=0):
        self.position = position
        self.points = points

    def move(self, count):
        self.position = ((self.position - 1 + count) % 10) + 1
        self.points += self.position

    def __repr__(self):
        return f"Player({self.position}, points={self.points})"


WINNING_SCORE = 1000


class Game:
    def __init__(self, die, player1, player2):
        self.die = die
        self.players = [player1, player2]
        self.player = cycle(self.players)

    def turn(self):
        player = next(self.player)
        player.move(self.die.roll() + self.die.roll() + self.die.roll())

    @property
    def winner(self):
        return max(self.players, key=attrgetter("points"))

    @property
    def loser(self):
        return min(self.players, key=attrgetter("points"))

    def play_until_one_player_reaches_1000(self):
        while all(p.points < WINNING_SCORE for p in self.players):
            self.turn()
        return self.winner


def find_points_loser_times_dice_rolls(starting_positions):
    game = Game(Die(), Player(starting_positions[0]), Player(starting_positions[1]))
    game.play_until_one_player_reaches_1000()
    return game.loser.points * game.die.n_rolls


if __name__ == "__main__":
    print(find_points_loser_times_dice_rolls((2, 8)))
