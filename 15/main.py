from __future__ import annotations
from typing import Iterator, NamedTuple
from queue import PriorityQueue
from functools import lru_cache


class Point(NamedTuple):
    col: int
    row: int

    def neighbors(self) -> Iterator[Point]:
        yield Point(self.col + 1, self.row)
        yield Point(self.col, self.row + 1)
        yield Point(self.col - 1, self.row)
        yield Point(self.col, self.row - 1)


def read_puzzle_input(filename: str) -> list[list[int]]:
    with open(filename) as stream:
        return [[int(r) for r in line] for line in stream.read().strip().split("\n")]


class CumSum:
    def __init__(self, maze):
        self.maze = maze
        self.max_row = len(maze) - 1
        self.max_col = len(maze[0]) - 1

    @lru_cache
    def flr(self, row: int, col: int) -> int:
        if row == col == 0:
            return 0
        if col < 0 or row < 0:
            return 1_000_000_000
        return self.maze[row][col] + min(
            (
                self.flr(row - 1, col),
                self.flr(row, col - 1),
            )
        )

    def find_lowest_risk(self):
        return self.flr(self.max_row, self.max_col)


class Dijkstra:
    def __init__(self, maze):
        self.maze = maze
        self.max_row = len(maze) - 1
        self.max_col = len(maze[0]) - 1

    def dijkstra(self, goal: Point):
        pq = PriorityQueue()
        pq.put((0, Point(0, 0)))
        visited: set[Point] = set()
        cum_risk: dict[Point, int] = {Point(0, 0): 0}
        # previous: dict[Point, Point] = {}
        while not pq.empty():
            risk, point = pq.get()
            visited.add(point)
            for neighbor in self.neighbors(point):
                if neighbor in visited:
                    continue
                r = risk + self[neighbor]
                if neighbor == goal:
                    return r
                if neighbor not in cum_risk:
                    cum_risk[neighbor] = r
                    # previous[neighbor] = point
                    pq.put((r, neighbor))
                elif r < cum_risk[neighbor]:
                    cum_risk[neighbor] = r
                    # previous[neighbor] = point

    def neighbors(self, point: Point) -> Iterator[Point]:
        for p in point.neighbors():
            try:
                self[p]
                yield p
            except IndexError:
                pass

    def __getitem__(self, point):
        return self.maze[point.row][point.col]

    def find_lowest_risk(self):
        return self.dijkstra(Point(self.max_col, self.max_row))


if __name__ == "__main__":
    print(CumSum(read_puzzle_input("test_input.txt")).find_lowest_risk())
    print(CumSum(read_puzzle_input("input.txt")).find_lowest_risk())
    print(Dijkstra(read_puzzle_input("test_input.txt")).find_lowest_risk())
    print(Dijkstra(read_puzzle_input("input.txt")).find_lowest_risk())
