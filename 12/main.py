from __future__ import annotations
from typing import Callable, Iterator, NamedTuple, Optional
from parse import parse
from collections import defaultdict


Filter = Callable[[str, tuple[str, ...]], bool]
Graph = dict[str, set[str]]
Path = tuple[str, ...]

START = "start"
END = "end"


class Connection(NamedTuple):
    start: str
    end: str


def construct_graph(connections: list[Connection]) -> Graph:
    caves: Graph = defaultdict(set)
    for con in connections:
        caves[con.start].add(con.end)
        caves[con.end].add(con.start)
    return caves


def find_paths(
    graph: Graph, dont_visit: Filter, cave: str = START, visited: Optional[Path] = None
) -> Iterator[Path]:
    visited = visited + (cave,) if visited else (cave,)
    if cave == END:
        yield visited
    else:
        for next_cave in graph[cave]:
            if dont_visit(next_cave, visited):
                continue
            yield from find_paths(graph, dont_visit, next_cave, visited)


def twice_visited(cave: str, path: Path) -> bool:
    return cave.islower() and cave in path


def twice_visited_2(cave: str, path: Path) -> bool:
    if not twice_visited(cave, path):
        return False
    if cave == START:
        return True
    if len(lower_caves := [n for n in path if n.islower()]) == len(set(lower_caves)):
        return False
    return True


def calc_n_paths(
    connections: list[Connection], dont_visit: Filter = twice_visited
) -> int:
    graph: Graph = construct_graph(connections)
    return sum(1 for _ in find_paths(graph, dont_visit=dont_visit))


def calc_n_paths_revisited(connections: list[Connection]) -> int:
    return calc_n_paths(connections, twice_visited_2)


def read_puzzle_input(filename: str) -> list[Connection]:
    with open(filename) as stream:
        connections: list[Connection] = []
        for line in stream.read().strip().split("\n"):
            p: dict[str, str] = parse("{cave1}-{cave2}", line).named
            connections.append(Connection(p["cave1"], p["cave2"]))
    return connections


if __name__ == "__main__":
    puzzle_input: list[Connection] = read_puzzle_input("input.txt")
    print(calc_n_paths(puzzle_input))
    print(calc_n_paths_revisited(puzzle_input))
