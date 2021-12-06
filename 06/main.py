from __future__ import annotations

# from collections import defaultdict
from typing import DefaultDict, Iterator, List, NamedTuple, Optional


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
    def __init__(self, list_of_ints):
        self.fishies = [Lanternfish(f) for f in list_of_ints]

    def evolve(self):
        new_fishies = self.fishies[:]
        for fish in self.fishies:
            if (new_fish := fish.evolve()):
                new_fishies.append(new_fish)
        self.fishies = new_fishies

    def __len__(self):
        return len(self.fishies)

    def __str__(self):
        return ",".join(str(f) for f in self.fishies)


def read_puzzle_input(filename: str) -> List[int]:
    with open(filename) as stream:
        return [int(n) for n in stream.readline().strip().split(",")]


if __name__ == "__main__":
    fishies: School = School(read_puzzle_input("input.txt"))
    for _ in range(80):
        fishies.evolve()
        print(fishies)
    print(len(fishies))

