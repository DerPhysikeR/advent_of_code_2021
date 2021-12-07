from __future__ import annotations
from typing import Callable, List, Optional, Tuple


def read_puzzle_input(filename: str) -> List[int]:
    with open(filename) as stream:
        return [int(n) for n in stream.readline().strip().split(",")]


def calc_fuel_cost(crabs: List[int], position: int) -> int:
    return sum(abs(c - position) for c in crabs)


def find_min_fuel_pos(
    crabs: List[int],
    fuel_cost_fun: Optional[Callable[[List[int], int], int]] = None,
) -> Tuple[int, int]:

    if fuel_cost_fun is None:
        fuel_cost_fun = calc_fuel_cost
    min_pos: int = min(crabs)
    fuel_cost: int = fuel_cost_fun(crabs, min(crabs))
    for pos in range(min(crabs), max(crabs) + 1):
        if (new_fuel_cost := fuel_cost_fun(crabs, pos)) < fuel_cost:
            fuel_cost = new_fuel_cost
            min_pos = pos
    return fuel_cost, min_pos


def calc_fuel_cost_2(crabs: List[int], position: int) -> int:
    return sum((n := abs(c - position)) * (n + 1) // 2 for c in crabs)


if __name__ == "__main__":
    puzzle_input: List[int] = read_puzzle_input("input.txt")
    print(find_min_fuel_pos(puzzle_input)[0])
    print(find_min_fuel_pos(puzzle_input, calc_fuel_cost_2)[0])
