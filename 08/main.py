from __future__ import annotations
from typing import Callable, Dict, List, Optional, Tuple, NamedTuple, FrozenSet, Set

Pattern = FrozenSet[str]


class Entry(NamedTuple):
    inp: List[Pattern]
    out: List[Pattern]


def parse_entry(line: str) -> Entry:
    inp_str, out_str = line.strip().split(" | ")
    return Entry(
        [frozenset(i) for i in inp_str.split()], [frozenset(o) for o in out_str.split()]
    )


def read_puzzle_input(filename: str) -> List[Entry]:
    with open(filename) as stream:
        return [parse_entry(l) for l in stream.read().strip().split("\n")]


def count_1478_in_output(entries: List[Entry]):
    out_len = [len(d) for e in entries for d in e.out]
    return out_len.count(2) + out_len.count(3) + out_len.count(4) + out_len.count(7)


DIGIT_MAP: Dict[Pattern, int] = {
    frozenset("abcefg"): 0,
    frozenset("cf"): 1,
    frozenset("acdeg"): 2,
    frozenset("acdfg"): 3,
    frozenset("bcdf"): 4,
    frozenset("abdfg"): 5,
    frozenset("abdefg"): 6,
    frozenset("acf"): 7,
    frozenset("abcdefg"): 8,
    frozenset("abcdfg"): 9,
}

INV_DIGIT_MAP: Dict[int, Pattern] = {v: k for k, v in DIGIT_MAP.items()}


LETTERS: FrozenSet[str] = frozenset("abcdefg")


def map_to_digit(pattern: Pattern) -> int:
    return DIGIT_MAP[pattern]


def translate_pattern(
    input_pattern: Pattern, mapping: Dict[Pattern, Pattern]
) -> Pattern:
    return mapping[input_pattern]


def convert_letter_map_to_map(letter_map: Dict[str, str]) -> Dict[Pattern, Pattern]:
    pattern_dict = {}
    for pattern in DIGIT_MAP:
        new_pattern = set()
        for letter in pattern:
            new_pattern.add(letter_map[letter])
        pattern_dict[pattern] = frozenset(new_pattern)
    return pattern_dict


PATTERN_LENGTH_TO_DIGIT = {2: 1, 4: 4, 3: 7, 7: 8}


def find_mapping(entry: Entry) -> Dict[Pattern, int]:
    e = set(entry.inp)
    m: Dict[int, Pattern] = {}
    while len(e) > 0:
        for pattern in e:
            try:
                if (lp := len(pattern)) in PATTERN_LENGTH_TO_DIGIT:
                    m[PATTERN_LENGTH_TO_DIGIT[lp]] = pattern
                    e.remove(pattern)
                    break
                if (m[8] - m[7]).issubset(pattern):
                    m[6] = pattern
                    e.remove(pattern)
                    break
                if (m[8] - m[7] - m[4]).issubset(pattern) and m[7].issubset(pattern):
                    m[0] = pattern
                    e.remove(pattern)
                    break
                if (m[8] - m[7] - m[4]).issubset(pattern) and not m[7].issubset(
                    pattern
                ):
                    m[2] = pattern
                    e.remove(pattern)
                    break
                if (m[4].union(m[7])).issubset(pattern):
                    m[9] = pattern
                    e.remove(pattern)
                    break
                if (m[4] - m[1]).issubset(pattern):
                    m[5] = pattern
                    e.remove(pattern)
                    break
                m[3] = pattern
                e.remove(pattern)
                break
            except KeyError:
                pass
    return {v: k for k, v in m.items()}


def get_output_val(entry: Entry) -> int:
    mapping: Dict[Pattern, int] = find_mapping(entry)
    digits: List[int] = [mapping[p] for p in entry.out]
    return 1000 * digits[0] + 100 * digits[1] + 10 * digits[2] + digits[3]


def get_output_val_sum(entries: List[Entry]) -> int:
    return sum(get_output_val(e) for e in entries)


if __name__ == "__main__":
    puzzle_input: List[Entry] = read_puzzle_input("input.txt")
    print(count_1478_in_output(puzzle_input))
    print(get_output_val_sum(puzzle_input))
