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


# class Maze:
#     def __init__(self, maze: list[list[int]]):
#         self.maze: dict[Point, int] = {
#             Point(ci, ri): risk
#             for ri, row in enumerate(maze)
#             for ci, risk in enumerate(row)
#         }
#         self.goal = Point(len(maze[0]) - 1, len(maze) - 1)
#
#     def neighbors(self, point: Point) -> Iterator[Point]:
#         for p in point.neighbors():
#             if p in self.maze:
#                 yield p
#
#     def find_shortest_path(self) -> tuple[Point, ...]:
#         return self.find_shortest_path_dijkstra()
#
#     def find_shortest_path_dijkstra(self) -> tuple[Point, ...]:
#         looked_at_all_neighbors: set[Point] = set()
#         previous: dict[Point, Point] = {}
#         risk_to_point: dict[Point, int] = {Point(0, 0): 0}
#         pq = PriorityQueue()
#         pq.put((0, Point(0, 0)))
#         while not pq.empty():
#             risk, point = pq.get()
#             looked_at_all_neighbors.add(point)
#             for neighbor in self.neighbors(point):
#                 if neighbor in looked_at_all_neighbors:
#                     continue
#                 cum_risk = risk + self.maze[neighbor]
#                 if neighbor not in risk_to_point:
#                     risk_to_point[neighbor] = cum_risk
#                     previous[neighbor] = point
#                     if neighbor == self.goal:
#                         return self.reconstruct_path(self.goal, previous)
#                 elif cum_risk < risk_to_point[neighbor]:
#                     risk_to_point[neighbor] = cum_risk
#                     previous[neighbor] = point
#                 # pq.put((cum_risk - neighbor.col - neighbor.row, neighbor)) -> A*
#                 pq.put((cum_risk, neighbor))
#         return self.reconstruct_path(self.goal, previous)
#
#     def reconstruct_path(
#         self, goal: Point, previous_dict: dict[Point, Point]
#     ) -> tuple[Point, ...]:
#         points = [goal]
#         point = goal
#         while point != Point(0, 0):
#             points.append(point := previous_dict[point])
#         return tuple(reversed(points))
#
#     def risk_of_path_without_start(self, path: tuple[Point, ...]) -> int:
#         return sum(self.maze[p] for p in path[1:])


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
                # find_lowest_risk(row + 1, col),
                # find_lowest_risk(row, col + 1),
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
