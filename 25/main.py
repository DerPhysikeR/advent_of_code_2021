from __future__ import annotations
from typing import NamedTuple
from enum import Enum


class Vector(NamedTuple):
    row: int
    col: int

    def __add__(self, other: Vector) -> Vector:
        return Vector(self.row + other.row, self.col + other.col)

    def __mod__(self, other: Vector) -> Vector:
        return Vector(self.row % other.row, self.col % other.col)


class Direction(Enum):
    EAST = Vector(0, 1)
    SOUTH = Vector(1, 0)


class Seacucumber:
    CHR_DIR_DICT = {">": Direction.EAST, "v": Direction.SOUTH}
    DIR_CHR_DICT = {v: k for k, v in CHR_DIR_DICT.items()}

    def __init__(self, direction: str):
        self.direction = self.CHR_DIR_DICT[direction]

    def __repr__(self):
        return f'Seacucumber("{str(self)}")'

    def __str__(self):
        return self.DIR_CHR_DICT[self.direction]


class Seafloor:
    def __init__(self, size: Vector, seacucumbers: dict[Vector, Seacucumber]):
        self.size = size
        self.seacucumbers = seacucumbers

    def _evolve_direction(self, direction: Direction):
        newcucumbers: dict[Vector, Seacucumber] = {}
        for position, sc in self.seacucumbers.items():
            if (dir := sc.direction) == direction:
                if self.is_free(np := (position + dir.value) % self.size):
                    newcucumbers[np] = sc
                else:
                    newcucumbers[position] = sc
            else:
                newcucumbers[position] = sc
        self.seacucumbers = newcucumbers

    def evolve(self):
        self._evolve_direction(Direction.EAST)
        self._evolve_direction(Direction.SOUTH)
        return self

    def evolve_until_gridlock(self):
        count = 0
        previous_seafloor: str = ""
        while previous_seafloor != (s := str(self)):
            previous_seafloor = s
            self.evolve()
            count += 1
        return count

    def is_free(self, position):
        return position not in self.seacucumbers

    def __repr__(self):
        return f"Seafloor(size={self.size}, seacucumbers={self.seacucumbers})"

    def __str__(self):
        result = []
        for irow in range(self.size.row):
            result.append([])
            for icol in range(self.size.col):
                if (pos := Vector(irow, icol)) in self.seacucumbers:
                    result[-1].append(str(self.seacucumbers[pos]))
                else:
                    result[-1].append(".")
        return "\n".join("".join(line) for line in result)

    @classmethod
    def from_file(cls, filename: str):
        irow: int = 0
        icol: int = 0
        seacucumbers = {}
        with open(filename) as stream:
            for irow, line in enumerate(stream):
                line = line.strip()
                for icol, ch in enumerate(line):
                    if ch == ".":
                        continue
                    else:
                        seacucumbers[Vector(irow, icol)] = Seacucumber(ch)
        return cls(Vector(irow + 1, icol + 1), seacucumbers)


if __name__ == "__main__":
    INPUT = "input.txt"
    print(Seafloor.from_file(INPUT).evolve_until_gridlock())
