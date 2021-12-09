from __future__ import annotations
from math import prod
from typing import List, NamedTuple, Set, Iterator, Dict


class Point(NamedTuple):
    row: int
    col: int

    def neighbors(self) -> Iterator[Point]:
        yield Point(self.row, self.col + 1)
        yield Point(self.row - 1, self.col)
        yield Point(self.row, self.col - 1)
        yield Point(self.row + 1, self.col)


class HeightMap:
    def __init__(self, heightmap: List[List[int]]):
        self.heightmap = heightmap

    def __getitem__(self, key):
        return self.heightmap[key]

    def __call__(self, point: Point) -> int:
        return self.heightmap[point.row][point.col]

    @property
    def width(self) -> int:
        return len(self.heightmap[0])

    @property
    def height(self) -> int:
        return len(self.heightmap)

    def neighbors(self, point: Point) -> Iterator[Point]:
        for n in point.neighbors():
            if (0 <= n.row < self.height) and (0 <= n.col < self.width):
                yield n

    def find_low_points(self) -> List[Point]:
        low_points: List[Point] = []
        for ri, row in enumerate(self.heightmap):
            for ci, height in enumerate(row):
                point: Point = Point(ri, ci)
                if all(height < self(n) for n in self.neighbors(point)):
                    low_points.append(point)
        return low_points

    def get_risk_level(self, point: Point) -> int:
        return self(point) + 1

    def find_basin_around(self, point: Point) -> Set[Point]:
        if self(point) == 9:
            return set()
        basin: Set[Point] = set([point])
        points_to_check: List[Point] = list(self.neighbors(point))
        points_checked: Set[Point] = set([point])
        for ptc in points_to_check:
            points_checked.add(ptc)
            if self(ptc) < 9:
                basin.add(ptc)
            else:
                continue
            for n in self.neighbors(ptc):
                if n not in points_checked:
                    points_to_check.append(n)
        return basin

    def find_all_basins(self) -> List[Set[Point]]:
        low_points = self.find_low_points()
        return [self.find_basin_around(lp) for lp in low_points]

    def product_of_3_largest_basin_sizes(self) -> int:
        basin_sizes: List[int] = sorted([len(b) for b in self.find_all_basins()])
        return prod(basin_sizes[-3:])

    def __str__(self) -> str:
        return "\n".join("".join(str(p) for p in row) for row in self.heightmap)

    def to_str_with_marked_basins(self):
        basins: List[Set[Point]] = self.find_all_basins()
        which_basin: Dict[Point, str] = {
            p: chr(i + 97) for i, basin in enumerate(basins) for p in basin
        }
        basin_map: List[List[str]] = []
        for ri, row in enumerate(self.heightmap):
            basin_map.append([])
            for ci, h in enumerate(row):
                basin_map[ri].append(which_basin.get(Point(ri, ci), str(h)))
        return "\n".join("".join(c for c in row) for row in basin_map)


def read_puzzle_input(filename: str) -> HeightMap:
    with open(filename) as stream:
        return HeightMap(
            [[int(n) for n in row] for row in stream.read().strip().split("\n")]
        )


if __name__ == "__main__":
    puzzle_input: HeightMap = read_puzzle_input("input.txt")
    print(puzzle_input.to_str_with_marked_basins())
    print(sum(puzzle_input.get_risk_level(p) for p in puzzle_input.find_low_points()))
    print(puzzle_input.product_of_3_largest_basin_sizes())
