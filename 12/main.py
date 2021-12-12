from __future__ import annotations
from typing import Callable, Iterator, NamedTuple
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
    nodes: Graph = defaultdict(set)
    for con in connections:
        nodes[con.start].add(con.end)
        nodes[con.end].add(con.start)
    return nodes


def find_paths(
    graph: Graph, dont_visit: Filter, node: str = START, visited: Path = None
) -> Iterator[Path]:
    visited = visited + (node,) if visited else (node,)
    if node == END:
        yield visited
    else:
        for n in graph[node]:
            if dont_visit(n, visited):
                continue
            yield from find_paths(graph, dont_visit, node=n, visited=visited)


def twice_visited(node: str, path: Path) -> bool:
    return node.islower() and node in path


def twice_visited_2(node: str, path: Path) -> bool:
    if not twice_visited(node, path):
        return False
    if node == START:
        return True
    if len(lower_nodes := [n for n in path if n.islower()]) == len(set(lower_nodes)):
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
            p: dict[str, str] = parse("{node1}-{node2}", line).named
            connections.append(Connection(p["node1"], p["node2"]))
    return connections


if __name__ == "__main__":
    puzzle_input: list[Connection] = read_puzzle_input("input.txt")
    print(calc_n_paths(puzzle_input))
    print(calc_n_paths_revisited(puzzle_input))
