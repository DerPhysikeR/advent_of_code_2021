from __future__ import annotations
from typing import Iterable, Iterator, NamedTuple
from parse import parse
from math import prod


class Cube(NamedTuple):
    x: int
    y: int
    z: int

    def __sub__(self, other):
        return Cube(self.x - other.x, self.y - other.y, self.z - other.z)


class Cuboid(NamedTuple):
    mincube: Cube
    maxcube: Cube

    def __iter__(self) -> Iterator[Cube]:
        for z in range(self.mincube.z, self.maxcube.z + 1):
            for y in range(self.mincube.y, self.maxcube.y + 1):
                for x in range(self.mincube.x, self.maxcube.x + 1):
                    yield Cube(x, y, z)

    def __contains__(self, cube: Cube) -> bool:
        return (
            (self.mincube.x <= cube.x <= self.maxcube.x)
            and (self.mincube.y <= cube.y <= self.maxcube.y)
            and (self.mincube.z <= cube.z <= self.maxcube.z)
        )

    def intersection(self, other: Cuboid) -> Cuboid:
        xmin: int = max(self.mincube.x, other.mincube.x)
        ymin: int = max(self.mincube.y, other.mincube.y)
        zmin: int = max(self.mincube.z, other.mincube.z)
        xmax: int = min(self.maxcube.x, other.maxcube.x)
        ymax: int = min(self.maxcube.y, other.maxcube.y)
        zmax: int = min(self.maxcube.z, other.maxcube.z)
        return Cuboid(Cube(xmin, ymin, zmin), Cube(xmax, ymax, zmax))

    @property
    def size(self):
        if any(d + 1 <= 0 for d in self.maxcube - self.mincube):
            return 0
        return prod(ma - mi + 1 for ma, mi in zip(self.maxcube, self.mincube))


class Instruction(NamedTuple):
    switch: bool
    cuboid: Cuboid


SWITCH = {"on": True, "off": False}


def read_puzzle_input(filename: str) -> list[Instruction]:
    instructions: list[Instruction] = []
    with open(filename) as stream:
        for line in stream:
            p = parse(
                "{switch:l} x={xmin:d}..{xmax:d},y={ymin:d}..{ymax:d},z={zmin:d}..{zmax:d}",
                line.strip(),
            ).named
            switch = SWITCH[p["switch"]]
            mincube = Cube(p["xmin"], p["ymin"], p["zmin"])
            maxcube = Cube(p["xmax"], p["ymax"], p["zmax"])
            instructions.append(Instruction(switch, Cuboid(mincube, maxcube)))
    return instructions


INIT_REGION = Cuboid(Cube(-50, -50, -50), Cube(50, 50, 50))


def reboot(instructions: list[Instruction]) -> set[Cube]:
    on_cubes: set[Cube] = set()
    for instruction in instructions:
        if instruction.switch:
            for cube in INIT_REGION.intersection(instruction.cuboid):
                on_cubes.add(cube)
        else:
            for cube in INIT_REGION.intersection(instruction.cuboid):
                try:
                    on_cubes.remove(cube)
                except KeyError:
                    pass
    return on_cubes


def on_cubes_in_init_region(instructions: list[Instruction]) -> int:
    return sum(1 if c in INIT_REGION else 0 for c in reboot(instructions))


def count_cubes(shapes: list[Cuboid]):
    sa = ShapeAdder()
    for shape in shapes:
        sa.add(shape)
    return sa.calc_size()


def count_after_instructions(instructions: list[Instruction]):
    sa = ShapeAdder()
    for switch, shape in instructions:
        if switch:
            sa.add(shape)
        else:
            sa.sub(shape)
    return sa.calc_size()


class ShapeAdder:
    def __init__(self):
        self.shapes: dict[tuple[int, ...], Cuboid] = {}
        self.idx = 0

    def add(self, cuboid):
        new_elements = {}
        for idx, shape in self.shapes.items():
            intersection = cuboid.intersection(shape)
            if intersection.size > 0:
                new_elements[idx + (self.idx,)] = intersection
        self.shapes[(self.idx,)] = cuboid
        self.shapes = {**self.shapes, **new_elements}
        self.idx += 1

    def sub(self, cuboid):
        new_elements = {}
        for idx, shape in self.shapes.items():
            intersection = cuboid.intersection(shape)
            if intersection.size > 0:
                new_elements[idx + (self.idx,)] = intersection
        self.shapes = {**self.shapes, **new_elements}
        self.idx += 1

    def calc_size(self):
        size = 0
        for idx, shape in self.shapes.items():
            size += shape.size * (-1) ** (len(idx) - 1)
        return size


if __name__ == "__main__":
    inp: list[Instruction] = read_puzzle_input("input.txt")
    print(on_cubes_in_init_region(inp))
    print(count_after_instructions(inp))
