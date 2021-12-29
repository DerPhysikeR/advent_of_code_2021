from __future__ import annotations
from typing import NamedTuple
from itertools import cycle, product
from operator import attrgetter
from collections import defaultdict


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


THROW_UNIVERSES = {3: 1, 4: 3, 5: 6, 6: 7, 7: 6, 8: 3, 9: 1}


class DPlayer(NamedTuple):
    position: int
    points: int

    def calc_new_position(self, throw: int) -> int:
        return ((self.position - 1 + throw) % 10) + 1


PDict = dict[DPlayer, int]


def evolve(players: PDict) -> PDict:
    new_players: defaultdict[DPlayer, int] = defaultdict(int)
    for player, player_universes in players.items():
        for throw, universes in THROW_UNIVERSES.items():
            new_position: int = player.calc_new_position(throw)
            new_points: int = player.points + new_position
            new_player: DPlayer = DPlayer(new_position, new_points)
            new_players[new_player] += player_universes * universes
    return new_players


def separate_by_points(players: PDict) -> tuple[PDict, PDict]:
    players_not_won: PDict = {}
    players_won: PDict = {}
    for player, universes in players.items():
        if player.points >= 21:
            players_won[player] = universes
        else:
            players_not_won[player] = universes
    return players_not_won, players_won


def find_winning_universe_counts(start1: int, start2: int) -> tuple[int, int]:
    player1: DPlayer = DPlayer(start1, 0)
    players1: PDict = {player1: 1}

    player2: DPlayer = DPlayer(start2, 0)
    players2: PDict = {player2: 1}

    p1_won_universes: int = 0
    p2_won_universes: int = 0

    p1turn: bool = True
    while players1 and players2:
        if p1turn:
            players1, won = separate_by_points(evolve(players1))
            p1_won_universes += sum(
                u1 * u2 for u1, u2 in product(won.values(), players2.values())
            )
            p1turn = False
        else:
            players2, won = separate_by_points(evolve(players2))
            p2_won_universes += sum(
                u1 * u2 for u1, u2 in product(players1.values(), won.values())
            )
            p1turn = True
    return p1_won_universes, p2_won_universes


if __name__ == "__main__":
    print(find_points_loser_times_dice_rolls((2, 8)))
    print(find_winning_universe_counts(2, 8))
