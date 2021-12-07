from __future__ import annotations
from typing import List, Tuple


def read_puzzle_input(filename: str) -> List[int]:
    with open(filename) as stream:
        return [int(n) for n in stream.readline().strip().split(",")]


def calc_fuel_cost(crabs: List[int], position: int) -> int:
    return sum(abs(c - position) for c in crabs)


def find_minimal_fuel_cost_and_position(crabs: List[int]) -> Tuple[int, int]:
    minimum_position = 0
    fuel_cost = calc_fuel_cost(crabs, 0)
    for position in range(min(crabs), max(crabs) + 1):
        if (new_fuel_cost := calc_fuel_cost(crabs, position)) < fuel_cost:
            fuel_cost = new_fuel_cost
            minimum_position = position
    return fuel_cost, minimum_position


if __name__ == "__main__":
    puzzle_input: List[int] = read_puzzle_input("input.txt")
    fuel_cost, minimum_position = find_minimal_fuel_cost_and_position(puzzle_input)
    print(fuel_cost)
