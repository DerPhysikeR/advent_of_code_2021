from __future__ import annotations
from typing import List
from enum import Enum


PAIRING = {"(": ")", "[": "]", "{": "}", "<": ">"}

ILLEGAL_SCORE = {")": 3, "]": 57, "}": 1197, ">": 25137}


def check_line(line: str) -> int:
    check_list = []
    for letter in line:
        if letter in "([{<":
            check_list.append(letter)
        elif letter == PAIRING[check_list[-1]]:
            check_list.pop()
        else:
            return ILLEGAL_SCORE[letter]
    if check_list:
        return 0
    return 0


def calc_corruptnes_score(lines: List[str]) -> int:
    return sum(check_line(line) for line in lines)


def read_puzzle_input(filename: str) -> List[str]:
    with open(filename) as stream:
        return stream.read().strip().split("\n")


if __name__ == "__main__":
    puzzle_input: List[str] = read_puzzle_input("input.txt")
    print(calc_corruptnes_score(puzzle_input))
