from __future__ import annotations
from typing import Dict, List, NamedTuple, FrozenSet, Set, Iterator


class Point(NamedTuple):
    row: int
    col: int

    def neighbors(self):
        yield Point(self.row, self.col + 1)
        yield Point(self.row - 1, self.col)
        yield Point(self.row, self.col - 1)
        yield Point(self.row + 1, self.col)


class HeightMap:
    def __init__(self, heightmap):
        self.heightmap = heightmap

    def __getitem__(self, key):
        return self.heightmap[key]

    @property
    def width(self):
        return len(self.heightmap[0])

    @property
    def height(self):
        return len(self.heightmap)

    def neighbors(self, point: Point) -> Iterator[Point]:
        for n in point.neighbors():
            if (0 <= n.row < self.height) and (0 <= n.col < self.width):
                yield n

    def find_low_points(self):
        low_points = []
        for ri, row in enumerate(self.heightmap):
            for ci, height in enumerate(row):
                point = Point(ri, ci)
                if all(
                    height < self.heightmap[n.row][n.col] for n in self.neighbors(point)
                ):
                    low_points.append(point)
        return low_points

    def get_risk_level(self, point: Point):
        return self.heightmap[point.row][point.col] + 1


def read_puzzle_input(filename: str) -> HeightMap:
    with open(filename) as stream:
        return HeightMap(
            [[int(n) for n in row] for row in stream.read().strip().split("\n")]
        )


if __name__ == "__main__":
    puzzle_input: HeightMap = read_puzzle_input("input.txt")
    print(sum(puzzle_input.get_risk_level(p) for p in puzzle_input.find_low_points()))
