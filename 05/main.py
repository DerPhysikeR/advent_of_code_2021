from collections import defaultdict, namedtuple
from typing import List, Iterator


Point = namedtuple("Point", "x, y")

Line = namedtuple("Line", "start, end")


def draw_line(line: Line) -> Iterator[Point]:
    if check_if_horizontal(line):
        step = 1 if line.start.x < line.end.x else -1
        for x in range(line.start.x, line.end.x + step, step):
            yield Point(x, line.start.y)
    elif check_if_vertical(line):
        step = 1 if line.start.y < line.end.y else -1
        for y in range(line.start.y, line.end.y + step, step):
            yield Point(line.start.x, y)
    else:
        raise NotImplementedError(
            "Only horizontal and vertical lines are currently possible"
        )


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
