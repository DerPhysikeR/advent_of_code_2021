from __future__ import annotations
from reprlib import repr
from typing import Iterable, NamedTuple
from parse import parse


class Point(NamedTuple):
    x: int
    y: int

    def __getitem__(self, coord: str) -> int:
        assert coord in "xy"
        return self.x if coord == "x" else self.y

    def change(self, coord: str, value: int) -> Point:
        assert coord in "xy"
        if coord == "x":
            return Point(value, self.y)
        return Point(self.x, value)


class Fold(NamedTuple):
    coord: str
    value: int


class Paper:
    def __init__(self, points: set[Point]):
        self.points: set[Point] = points

    def folds(self, folds: Iterable[Fold]) -> Paper:
        paper: Paper = self
        for fold in folds:
            paper = paper.fold(fold)
        return paper

    def fold(self, fold: Fold) -> Paper:
        points: set[Point] = set()
        for point in self.points:
            if point[fold.coord] < fold.value:
                points.add(point)
            else:
                new_value: int = 2 * fold.value - point[fold.coord]
                points.add(point.change(fold.coord, new_value))
        return Paper(points)

    def __len__(self) -> int:
        return len(self.points)

    def __repr__(self) -> str:
        return f"Paper(points={repr(self.points)})"

    def min_max(self) -> tuple[Point, Point]:
        min_x: int = min(p.x for p in self.points)
        min_y: int = min(p.y for p in self.points)
        max_x: int = max(p.x for p in self.points)
        max_y: int = max(p.y for p in self.points)
        return Point(min_x, min_y), Point(max_x, max_y)

    def __str__(self) -> str:
        minp: Point
        maxp: Point
        minp, maxp = self.min_max()
        rows: list[str] = []
        for y in range(minp.y, maxp.y + 1):
            rows.append(
                "".join(
                    "#" if Point(x, y) in self.points else "."
                    for x in range(minp.x, maxp.x + 1)
                )
            )
        return "\n".join(rows)


def read_puzzle_input(filename: str) -> tuple[set[Point], list[Fold]]:
    with open(filename) as stream:
        points = set()
        for line in stream:
            if line == "\n":
                break
            points.add(Point(*parse("{:d},{:d}", line.strip()).fixed))
        folds = []
        for line in stream:
            folds.append(Fold(*parse("fold along {:l}={:d}", line.strip()).fixed))
    return points, folds


if __name__ == "__main__":
    points: set[Point]
    folds: list[Fold]
    points, folds = read_puzzle_input("input.txt")
    paper: Paper = Paper(points)
    print(len(paper.fold(folds[0])))
    print(paper.folds(folds))
