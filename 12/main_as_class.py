from __future__ import annotations
from typing import Callable, Iterator, NamedTuple, Optional
from parse import parse
from collections import defaultdict


Filter = Callable[[str, tuple[str, ...]], bool]
Path = tuple[str, ...]

START = "start"
END = "end"


class Connection(NamedTuple):
    start: str
    end: str


class Graph:
    def __init__(self, connections: list[Connection]):
        self.caves: dict[str, set[str]] = defaultdict(set)
        for con in connections:
            self[con.start].add(con.end)
            self[con.end].add(con.start)

    def __getitem__(self, cave):
        return self.caves[cave]

    def paths(
        self, dont_visit: Filter, cave: str = START, visited: Optional[Path] = None
    ) -> Iterator[Path]:
        visited = visited + (cave,) if visited else (cave,)
        if cave == END:
            yield visited
        else:
            for next_cave in self[cave]:
                if dont_visit(next_cave, visited):
                    continue
                yield from self.paths(dont_visit, next_cave, visited)

    def calc_n_paths(self) -> int:
        return self.sum_paths(self.twice_visited)

    def calc_n_paths_revisited(self) -> int:
        return self.sum_paths(self.twice_visited_2)

    def sum_paths(self, dont_visit: Filter) -> int:
        return sum(1 for _ in self.paths(dont_visit))

    @staticmethod
    def twice_visited(cave: str, path: Path) -> bool:
        return cave.islower() and cave in path

    @classmethod
    def twice_visited_2(cls, cave: str, path: Path) -> bool:
        if not cls.twice_visited(cave, path):
            return False
        if cave == START:
            return True
        if len(lower_caves := [n for n in path if n.islower()]) == len(
            set(lower_caves)
        ):
            return False
        return True

    @classmethod
    def from_file(cls, filename: str) -> Graph:
        with open(filename) as stream:
            connections: list[Connection] = []
            for line in stream.read().strip().split("\n"):
                p: dict[str, str] = parse("{cave1}-{cave2}", line).named
                connections.append(Connection(p["cave1"], p["cave2"]))
        return cls(connections)


if __name__ == "__main__":
    graph: Graph = Graph.from_file("input.txt")
    print(graph.calc_n_paths())
    print(graph.calc_n_paths_revisited())
