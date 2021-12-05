from collections import defaultdict, namedtuple
from typing import List, Iterator


class Point(namedtuple("Point", "x, y")):
    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y)

    def __mul__(self, number):
        return Point(self.x * number, self.y * number)

    def __rmul__(self, number):
        return self * number


Line = namedtuple("Line", "start, end")


def find_delta(c1: int, c2: int) -> int:
    if c1 == c2:
        return 0
    if c1 < c2:
        return 1
    return -1


def draw_line(line: Line) -> Iterator[Point]:
    dx = find_delta(line.start.x, line.end.x)
    dy = find_delta(line.start.y, line.end.y)
    delta = Point(dx, dy)
    point = line.start
    while point != line.end:
        yield point
        point += delta
    yield line.end


def parse_line(line: str) -> Line:
    points = line.split(" -> ")
    points = [[int(c) for c in point.split(",")] for point in points]
    return Line(*[Point(*p) for p in points])


def read_puzzle_input(filename: str) -> List[Line]:
    with open(filename) as stream:
        return [parse_line(line) for line in stream.read().strip().split("\n")]


def check_if_horizontal(line: Line) -> bool:
    return line.start.y == line.end.y


def check_if_vertical(line: Line) -> bool:
    return line.start.x == line.end.x


def check_if_orthogonal(line: Line):
    return check_if_horizontal(line) or check_if_vertical(line)


def count_overlapping_points(lines: List[Line]):
    board = defaultdict(int)
    for line in lines:
        for point in draw_line(line):
            board[point] += 1
    return len([p for p, c in board.items() if c > 1])


if __name__ == "__main__":
    lines = read_puzzle_input("input.txt")
    orthogonal_lines = [line for line in lines if check_if_orthogonal(line)]
    print(count_overlapping_points(orthogonal_lines))
    print(count_overlapping_points(lines))
