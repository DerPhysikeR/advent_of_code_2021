from __future__ import annotations
from typing import Callable, List, Tuple, Optional
from enum import Enum


class LineState(Enum):
    VALID = "valid"
    CORRUPT = "corrupt"
    INCOMPLETE = "incomplete"


PAIRING = {"(": ")", "[": "]", "{": "}", "<": ">"}


def check_line(
    line: str, score_fun: Callable[[LineState, List[str], Optional[str]], int]
) -> int:
    check_list = []
    for letter in line:
        if letter in "([{<":
            check_list.append(letter)
        elif letter == PAIRING[check_list[-1]]:
            check_list.pop()
        else:
            return score_fun(LineState.CORRUPT, check_list, letter)
    if check_list:
        return score_fun(LineState.INCOMPLETE, check_list)
    return score_fun(LineState.VALID, check_list)


ILLEGAL_SCORE = {")": 3, "]": 57, "}": 1197, ">": 25137}


def corruptness_scoring_function(
    line_state: LineState, _: List[str], letter: Optional[str] = ""
) -> int:
    if line_state == LineState.CORRUPT:
        return ILLEGAL_SCORE[letter]
    return 0


def calc_corruptnes_score(lines: List[str]) -> int:
    return sum(check_line(line, corruptness_scoring_function) for line in lines)


def read_puzzle_input(filename: str) -> List[str]:
    with open(filename) as stream:
        return stream.read().strip().split("\n")


if __name__ == "__main__":
    puzzle_input: List[str] = read_puzzle_input("input.txt")
    print(calc_corruptnes_score(puzzle_input))
