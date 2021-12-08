from __future__ import annotations
from typing import Dict, List, NamedTuple, FrozenSet, Set, Tuple

Pattern = FrozenSet[str]


class Entry(NamedTuple):
    inp: List[Pattern]
    out: List[Pattern]


def parse_entry(line: str) -> Entry:
    inp: str
    out: str
    inp, out = line.strip().split(" | ")
    return Entry(
        [frozenset(i) for i in inp.split()], [frozenset(o) for o in out.split()]
    )


def read_puzzle_input(filename: str) -> List[Entry]:
    with open(filename) as stream:
        return [parse_entry(l) for l in stream.read().strip().split("\n")]


def count_1478_in_output(entries: List[Entry]):
    out_len: List[int] = [len(d) for e in entries for d in e.out]
    return out_len.count(2) + out_len.count(3) + out_len.count(4) + out_len.count(7)


PATTERN_LENGTH_TO_DIGIT = {2: 1, 4: 4, 3: 7, 7: 8}


def apply_rules(m: Dict[int, Pattern], pattern: Pattern):
    if (lp := len(pattern)) in PATTERN_LENGTH_TO_DIGIT:
        m[PATTERN_LENGTH_TO_DIGIT[lp]] = pattern
        return pattern
    if (m[8] - m[7]).issubset(pattern):
        m[6] = pattern
        return pattern
    if (m[8] - m[7] - m[4]).issubset(pattern) and m[7].issubset(pattern):
        m[0] = pattern
        return pattern
    if (m[8] - m[7] - m[4]).issubset(pattern) and not m[7].issubset(pattern):
        m[2] = pattern
        return pattern
    if (m[4].union(m[7])).issubset(pattern):
        m[9] = pattern
        return pattern
    if (m[4] - m[1]).issubset(pattern):
        m[5] = pattern
        return pattern
    m[3] = pattern
    return pattern


def find_mapping(entry: Entry) -> Dict[Pattern, int]:
    patterns_to_check: Set[Pattern] = set(entry.inp)
    mapping: Dict[int, Pattern] = {}
    while len(patterns_to_check) > 0:
        for pattern in patterns_to_check:
            try:
                patterns_to_check.remove(apply_rules(mapping, pattern))
                break
            except KeyError:
                pass
    return {v: k for k, v in mapping.items()}


def get_output_val(entry: Entry) -> int:
    mapping: Dict[Pattern, int] = find_mapping(entry)
    digits: List[int] = [mapping[p] for p in entry.out]
    return sum(10 ** (3 - i) * d for i, d in enumerate(digits))


def get_output_val_sum(entries: List[Entry]) -> int:
    return sum(get_output_val(e) for e in entries)


if __name__ == "__main__":
    puzzle_input: List[Entry] = read_puzzle_input("input.txt")
    print(count_1478_in_output(puzzle_input))
    print(get_output_val_sum(puzzle_input))
