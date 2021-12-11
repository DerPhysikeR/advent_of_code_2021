from __future__ import annotations
from typing import Dict, List, Tuple, NamedTuple, Iterator, Set
from itertools import product, count


class Point(NamedTuple):
    row: int
    col: int

    def neighbors(self) -> Iterator[Point]:
        d: Tuple[int, int, int] = (-1, 0, 1)
        for dr, dc in product(d, d):
            if dr == dc == 0:
                continue
            yield Point(self.row + dr, self.col + dc)


class Grid:
    def __init__(self, energy_levels: List[List[int]]):
        self.width = len(energy_levels[0])
        self.height = len(energy_levels)
        self.energy_levels = {}
        for ri, row in enumerate(energy_levels):
            for ci, el in enumerate(row):
                self.energy_levels[Point(ri, ci)] = el

    def _evolve(self) -> int:
        # increment by one
        for point in self.energy_levels:
            self.energy_levels[point] += 1
        # find all flashes
        flashes: Set[Point] = set([p for p, el in self.items() if el > 9])
        new_flashes = flashes.copy()
        while new_flashes:
            for fl in new_flashes:
                for neighbor in fl.neighbors():
                    try:
                        self[neighbor] += 1
                    except KeyError:
                        pass
            flashes = flashes.union(new_flashes)
            new_flashes = set()
            for p, el in self.items():
                if el > 9 and p not in flashes:
                    new_flashes.add(p)
        # set flashes to 0
        for fl in flashes:
            self[fl] = 0

        return len(flashes)

    def evolve(self, n_steps: int):
        n_flashes: int = 0
        for _ in range(n_steps):
            n_flashes += self._evolve()
        return n_flashes

    def find_first_sync_flash(self) -> int:
        count = 1
        while len(self.energy_levels) > self._evolve():
            count += 1
        return count

    def items(self):
        return self.energy_levels.items()

    def __getitem__(self, point: Point):
        return self.energy_levels[point]

    def __setitem__(self, point: Point, value: int):
        self.energy_levels[point] = value

    def __str__(self):
        return "\n".join(
            "".join(str(self[Point(ri, ci)]) for ci in range(self.width))
            for ri in range(self.height)
        )


def read_puzzle_input(filename: str) -> List[List[int]]:
    with open(filename) as stream:
        return [[int(n) for n in row] for row in stream.read().strip().split("\n")]


if __name__ == "__main__":
    puzzle_input: List[List[int]] = read_puzzle_input("input.txt")
    print(Grid(puzzle_input).evolve(100))
    print(Grid(puzzle_input).find_first_sync_flash())
