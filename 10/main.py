from __future__ import annotations
from typing import Dict, List, Tuple
from enum import auto, Enum


class LineState(Enum):
    VALID = auto()
    CORRUPT = auto()
    INCOMPLETE = auto()


PAIRING: Dict[str, str] = {"(": ")", "[": "]", "{": "}", "<": ">"}


def score_line(line: str) -> Tuple[LineState, int]:
    check_list: List[str] = []
    for letter in line:
        if letter in "([{<":
            check_list.append(letter)
        elif letter == PAIRING[check_list[-1]]:
            check_list.pop()
        else:
            return LineState.CORRUPT, score_corrupt_line(letter)
    if check_list:
        return LineState.INCOMPLETE, score_incomp_line(check_list)
    return LineState.VALID, 0


CORRUPT_SCORE: Dict[str, int] = {")": 3, "]": 57, "}": 1197, ">": 25137}


def score_corrupt_line(letter: str) -> int:
    return CORRUPT_SCORE[letter]


def calc_corruptness_score(lines: List[str]) -> int:
    score: int = 0
    for line in lines:
        state: LineState
        line_score: int
        state, line_score = score_line(line)
        if state == LineState.CORRUPT:
            score += line_score
    return score


INCOMP_SCORE = {")": 1, "]": 2, "}": 3, ">": 4}


def score_incomp_line(remaining_parens: List[str]) -> int:
    score: int = 0
    for closing_paren in [PAIRING[p] for p in reversed(remaining_parens)]:
        score = 5 * score + INCOMP_SCORE[closing_paren]
    return score


def calc_incompleteness_score(lines: List[str]) -> int:
    scores: List[int] = []
    for line in lines:
        state, line_score = score_line(line)
        if state == LineState.INCOMPLETE:
            scores.append(line_score)
    return sorted(scores)[(len(scores) - 1) // 2]


def read_puzzle_input(filename: str) -> List[str]:
    with open(filename) as stream:
        return stream.read().strip().split("\n")


if __name__ == "__main__":
    puzzle_input: List[str] = read_puzzle_input("input.txt")
    print(calc_corruptness_score(puzzle_input))
    print(calc_incompleteness_score(puzzle_input))
