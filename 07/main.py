from __future__ import annotations
from typing import List, Tuple, Callable


def read_puzzle_input(filename: str) -> List[int]:
    with open(filename) as stream:
        return [int(n) for n in stream.readline().strip().split(",")]


def calc_fuel_cost(crabs: List[int], position: int) -> int:
    return sum(abs(c - position) for c in crabs)


def find_minimal_fuel_cost_and_position(
    crabs: List[int], fuel_cost_function=None
) -> Tuple[int, int]:

    if fuel_cost_function is None:
        fuel_cost_function = calc_fuel_cost
    minimum_position = min(crabs)
    fuel_cost = fuel_cost_function(crabs, min(crabs))
    for position in range(min(crabs), max(crabs) + 1):
        if (new_fuel_cost := fuel_cost_function(crabs, position)) < fuel_cost:
            fuel_cost = new_fuel_cost
            minimum_position = position
    return fuel_cost, minimum_position


def calc_fuel_cost_correctly(crabs: List[int], position: int) -> int:
    return sum((n := abs(c - position)) * (n + 1) // 2 for c in crabs)


if __name__ == "__main__":
    puzzle_input: List[int] = read_puzzle_input("input.txt")
    fuel_cost, minimum_position = find_minimal_fuel_cost_and_position(puzzle_input)
    print(fuel_cost)
    fuel_cost, minimum_position = find_minimal_fuel_cost_and_position(
        puzzle_input, calc_fuel_cost_correctly
    )
    print(fuel_cost)
