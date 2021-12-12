from __future__ import annotations
from typing import NamedTuple, Optional
from parse import parse

from collections import defaultdict


class Connection(NamedTuple):
    start: str
    end: str


def construct_graph(connections: list[Connection]) -> dict[str, set[str]]:
    nodes: defaultdict[str, set[str]] = defaultdict(set)
    for con in connections:
        nodes[con.start].add(con.end)
        nodes[con.end].add(con.start)
    return dict(nodes)


def find_paths(
    graph: dict[str, set[str]], node: str = "start", visited: tuple[str, ...] = None
):
    visited = visited + (node,) if visited else (node,)
    if node == "end":
        yield visited
    else:
        for n in graph[node]:
            if n.islower() and n in visited:
                continue
            yield from find_paths(graph, n, visited)


def twice_visited(path: tuple[str, ...], n: str) -> bool:
    if n not in path:
        return False
    if n == "start":
        return True
    lower_nodes = [n for n in path if n.islower()]
    set_lower_nodes = set(lower_nodes)
    if len(set_lower_nodes) == len(lower_nodes):
        return False
    return True


def find_paths_revisited(
    graph: dict[str, set[str]], node: str = "start", visited: tuple[str, ...] = None
):
    visited = visited + (node,) if visited else (node,)
    if node == "end":
        yield visited
    else:
        for n in graph[node]:
            if n.islower() and twice_visited(visited, n):
                continue
            yield from find_paths_revisited(graph, n, visited)


def calc_n_paths(connections: list[Connection]):
    graph: dict[str, set[str]] = construct_graph(connections)
    return sum(1 for _ in find_paths(graph))


def calc_n_paths_revisited(connections: list[Connection]):
    graph: dict[str, set[str]] = construct_graph(connections)
    return sum(1 for _ in find_paths_revisited(graph))


def read_puzzle_input(filename: str) -> list[Connection]:
    with open(filename) as stream:
        connections = []
        for line in stream.read().strip().split("\n"):
            p: dict = parse("{start}-{end}", line).named
            connections.append(Connection(p["start"], p["end"]))
    return connections


if __name__ == "__main__":
    puzzle_input: list[Connection] = read_puzzle_input("input.txt")
    print(calc_n_paths(puzzle_input))
    print(calc_n_paths_revisited(puzzle_input))
