from __future__ import annotations
from typing import Callable, List, Optional, Tuple, NamedTuple


class Entry(NamedTuple):
    inp: List[str]
    out: List[str]


def parse_entry(line: str) -> Entry:
    inp_str, out_str = line.strip().split(" | ")
    return Entry(inp_str.split(), out_str.split())


def read_puzzle_input(filename: str) -> List[Entry]:
    with open(filename) as stream:
        return [parse_entry(l) for l in stream.read().strip().split("\n")]


def count_1478_in_output(entries: List[Entry]):
    out_len = [len(d) for e in entries for d in e.out]
    return out_len.count(2) + out_len.count(3) + out_len.count(4) + out_len.count(7)


if __name__ == "__main__":
    puzzle_input: List[Entry] = read_puzzle_input("input.txt")
    print(count_1478_in_output(puzzle_input))
