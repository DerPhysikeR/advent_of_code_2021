from __future__ import annotations
from typing import List


class School:
    def __init__(self, list_of_ints: List[int]):
        self.fishies: dict[int, int] = {n: list_of_ints.count(n) for n in range(8 + 1)}

    def evolve(self, days: int = 1) -> School:
        for _ in range(days):
            new_fishies: dict[int, int] = {n: 0 for n in range(8 + 1)}
            for age, count in self.fishies.items():
                if age == 0:
                    new_fishies[8] += count
                    new_fishies[6] += count
                else:
                    new_fishies[age - 1] += count
            self.fishies = new_fishies
        return self

    def __len__(self) -> int:
        return sum(self.fishies.values())

    def __repr__(self) -> str:
        return f"School({self.fishies})"


def read_puzzle_input(filename: str) -> List[int]:
    with open(filename) as stream:
        return [int(n) for n in stream.readline().strip().split(",")]


if __name__ == "__main__":
    puzzle_input: List[int] = read_puzzle_input("input.txt")
    print(len(School(puzzle_input).evolve(80)))
    print(len(School(puzzle_input).evolve(256)))
