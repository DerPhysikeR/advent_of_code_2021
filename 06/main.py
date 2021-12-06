from __future__ import annotations

# from collections import defaultdict
from typing import DefaultDict, Iterator, List, NamedTuple, Optional, Dict


class Lanternfish:
    def __init__(self, start_timer=8):
        self.timer = start_timer

    def evolve(self) -> Optional[Lanternfish]:
        if self.timer > 0:
            self.timer -= 1
            return None
        self.timer = 6
        return Lanternfish()

    def __str__(self):
        return str(self.timer)


class School:
    def __init__(self, list_of_ints: List[int]):
        self.fishies: dict[int, int] = {n: list_of_ints.count(n) for n in range(8 + 1)}

    def evolve(self):
        new_fishies: dict[int, int] = {n: 0 for n in range(8 + 1)}
        for age, count in self.fishies.items():
            if age == 0:
                new_fishies[8] += count
                new_fishies[6] += count
            else:
                new_fishies[age - 1] += count
        self.fishies = new_fishies

    def __len__(self):
        return sum(v for v in self.fishies.values())

    def __str__(self):
        return str(self.fishies)


def read_puzzle_input(filename: str) -> List[int]:
    with open(filename) as stream:
        return [int(n) for n in stream.readline().strip().split(",")]


if __name__ == "__main__":
    fishies: School = School(read_puzzle_input("input.txt"))
    for _ in range(80):
        fishies.evolve()
        print(fishies)
    print(len(fishies))

    fishies: School = School(read_puzzle_input("input.txt"))
    for _ in range(256):
        fishies.evolve()
        print(fishies)
    print(len(fishies))
