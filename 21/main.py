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


PDict = dict[DPlayer, int]


def evolve(players: PDict):
    new_players: defaultdict[DPlayer, int] = defaultdict(int)
    for player, puni in players.items():
        for throw, universes in THROW_UNIVERSES.items():
            new_position = calc_new_position(player.position, throw)
            new_points = player.points + new_position
            new_player = DPlayer(new_position, new_points)
            new_players[new_player] += puni * universes
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


def calc_new_position(position, throw):
    return ((position - 1 + throw) % 10) + 1


def find_winning_universe_counts(starting_positions):
    player1 = DPlayer(starting_positions[0], 0)
    players1 = {player1: 1}

    player2 = DPlayer(starting_positions[1], 0)
    players2 = {player2: 1}

    p1_won_universes = 0
    p2_won_universes = 0

    p1turn = True
    while players1 and players2:
        if p1turn:
            players1 = evolve(players1)
            players1, won = separate_by_points(players1)
            p1_won_universes += sum(
                u1 * u2 for u1, u2 in product(won.values(), players2.values())
            )
            p1turn = False
        else:
            players2 = evolve(players2)
            players2, won = separate_by_points(players2)
            p2_won_universes += sum(
                u1 * u2 for u1, u2 in product(players1.values(), won.values())
            )
            p1turn = True
    return p1_won_universes, p2_won_universes


if __name__ == "__main__":
    print(find_points_loser_times_dice_rolls((2, 8)))
    print(find_winning_universe_counts((2, 8)))
