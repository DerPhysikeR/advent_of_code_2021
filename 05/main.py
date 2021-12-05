from __future__ import annotations
from collections import defaultdict
from typing import DefaultDict, Iterator, List, NamedTuple
from parse import parse


class Point(NamedTuple):
    x: int
    y: int

    def __add__(self, other: Point):
        return Point(self.x + other.x, self.y + other.y)


class Line(NamedTuple):
    start: Point
    end: Point

    def draw(self) -> Iterator[Point]:
        dx: int = find_delta(self.start.x, self.end.x)
        dy: int = find_delta(self.start.y, self.end.y)
        delta: Point = Point(dx, dy)
        point: Point = self.start
        while point != self.end:
            yield point
            point += delta
        yield self.end


def find_delta(start: int, end: int) -> int:
    if start < end:
        return 1
    if start > end:
        return -1
    return 0


def parse_line(line: str) -> Line:
    p: dict = parse("{sx:d},{sy:d} -> {ex:d},{ey:d}", line).named
    return Line(Point(p["sx"], p["sy"]), Point(p["ex"], p["ey"]))


def read_puzzle_input(filename: str) -> List[Line]:
    with open(filename) as stream:
        return [parse_line(line) for line in stream.read().strip().split("\n")]


def check_if_orthogonal(line: Line):
    return line.start.x == line.end.x or line.start.y == line.end.y


def count_overlapping_points(lines: List[Line]):
    board: DefaultDict[Point, int] = defaultdict(int)
    for line in lines:
        for point in line.draw():
            board[point] += 1
    return len([p for p, c in board.items() if c > 1])


if __name__ == "__main__":
    lines: List[Line] = read_puzzle_input("input.txt")
    orthogonal_lines: List[Line] = [line for line in lines if check_if_orthogonal(line)]
    print(count_overlapping_points(orthogonal_lines))
    print(count_overlapping_points(lines))
