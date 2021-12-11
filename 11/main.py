from __future__ import annotations
from collections.abc import ItemsView, Iterable, Iterator
from itertools import product
from typing import NamedTuple
from time import sleep


class Point(NamedTuple):
    row: int
    col: int

    def neighbors(self) -> Iterator[Point]:
        d: tuple[int, int, int] = (-1, 0, 1)
        for dr, dc in product(d, d):
            if not dr == dc == 0:
                yield Point(self.row + dr, self.col + dc)


class Grid:
    def __init__(self, energy_levels: list[list[int]]):
        self.width: int = len(energy_levels[0])
        self.height: int = len(energy_levels)
        self.energy_levels: dict[Point, int] = {}
        for ri, row in enumerate(energy_levels):
            for ci, el in enumerate(row):
                self.energy_levels[Point(ri, ci)] = el

    def _evolve(self) -> int:
        self.increment_by_one()
        flashes: set[Point] = self.find_flashes()
        new_flashes: set[Point] = flashes.copy()
        while new_flashes:
            new_flash_neighbors: Iterator[Point] = (
                n for fl in new_flashes for n in fl.neighbors()
            )
            self.increment_by_one(new_flash_neighbors)
            flashes = flashes.union(new_flashes)
            new_flashes = self.find_flashes().difference(flashes)
        self.nullify(flashes)
        return len(flashes)

    def find_flashes(self) -> set[Point]:
        return set([p for p, el in self.items() if el > 9])

    def increment_by_one(self, points: Iterable[Point] = None):
        if points is None:
            points = self.energy_levels.keys()
        for point in points:
            try:
                self[point] += 1
            except KeyError:
                pass

    def nullify(self, points: set[Point]):
        for point in points:
            self[point] = 0

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

    def items(self) -> ItemsView[Point, int]:
        return self.energy_levels.items()

    def __getitem__(self, point: Point) -> int:
        return self.energy_levels[point]

    def __setitem__(self, point: Point, value: int) -> None:
        self.energy_levels[point] = value

    def __str__(self) -> str:
        rows = []
        for ri in range(self.height):
            rows.append("".join(str(self[Point(ri, ci)]) for ci in range(self.width)))
        return "\n".join(rows)


def animate_grid(grid: Grid, dt: int):
    print(grid)
    while True:
        grid.evolve(1)
        sleep(dt)
        print("-" * grid.width)
        print(grid)


def read_puzzle_input(filename: str) -> list[list[int]]:
    with open(filename) as stream:
        return [[int(n) for n in row] for row in stream.read().strip().split("\n")]


if __name__ == "__main__":
    puzzle_input: list[list[int]] = read_puzzle_input("input.txt")
    print(Grid(puzzle_input).evolve(100))
    print(Grid(puzzle_input).find_first_sync_flash())
    animate_grid(Grid(puzzle_input), 1)
