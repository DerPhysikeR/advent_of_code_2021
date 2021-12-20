from __future__ import annotations
from typing import Iterator, NamedTuple, Iterable
from math import sqrt
from parse import parse
from itertools import takewhile
from operator import itemgetter
from enum import Enum


class Axis(Enum):
    x = 0
    y = 1
    z = 2


class Point(NamedTuple):
    x: int
    y: int
    z: int

    def distance(self, other: Point) -> float:
        return sqrt(
            (self.x - other.x) ** 2 + (self.y - other.y) ** 2 + (self.z - other.z) ** 2
        )

    def manhatten(self, other: Point) -> int:
        return (
            abs(self.x - other.x) + abs(self.y - other.y) + abs(self.z - other.z)
        )

    def __add__(self, other: Point) -> Point:
        return Point(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other: Point) -> Point:
        return Point(self.x - other.x, self.y - other.y, self.z - other.z)

    def __neg__(self) -> Point:
        return Point(-self.x, -self.y, -self.z)

    def rotate(self, axis: Axis) -> Point:
        if axis == Axis.x:
            return Point(self.x, -self.z, self.y)
        if axis == Axis.y:
            return Point(self.z, self.y, -self.x)
        if axis == Axis.z:
            return Point(-self.y, self.x, self.z)
        raise ValueError(f"Invalid Axis {axis}")


MAX_DIST = 1000
MAX_POINT = Point(1000, 1000, 1000)
MIN_POINT = Point(-1000, -1000, -1000)


class Cube:
    def __init__(
        self,
        beacons: Iterable[Point],
        min_point: Point = MIN_POINT,
        max_point: Point = MAX_POINT,
    ):
        self.min_point = min_point
        self.max_point = max_point
        self.beacons: list[Point] = [b for b in beacons if b in self]

    @property
    def scanner(self) -> Point:
        return self.min_point + MAX_POINT

    def calc_distances(self) -> list[set[float]]:
        distances: list[set[float]] = []
        for pa in self.beacons:
            distances.append(set(pa.distance(pb) for pb in self.beacons))
        return distances

    def calc_intersect_min_max(self, other: Cube):
        minx = max(self.min_point.x, other.min_point.x)
        miny = max(self.min_point.y, other.min_point.y)
        minz = max(self.min_point.z, other.min_point.z)
        maxx = min(self.max_point.x, other.max_point.x)
        maxy = min(self.max_point.y, other.max_point.y)
        maxz = min(self.max_point.z, other.max_point.z)
        return Point(minx, miny, minz), Point(maxx, maxy, maxz)

    def count_equal_beacons(self, other: Cube):
        return len(set(self.beacons).intersection(set(other.beacons)))

    def fits(self, other: Cube):
        min_point, max_point = self.calc_intersect_min_max(other)
        cube = Cube(self.beacons, min_point, max_point)
        other_cube = Cube(other.beacons, min_point, max_point)
        return set(cube.beacons) == set(other_cube.beacons)

    def __contains__(self, beacon: Point) -> bool:
        return (
            (self.min_point.x <= beacon.x <= self.max_point.x)
            and (self.min_point.y <= beacon.y <= self.max_point.y)
            and (self.min_point.z <= beacon.z <= self.max_point.z)
        )

    def similarity(self, other: Cube):
        similarities = []
        for i, sd in enumerate(self.calc_distances()):
            for j, od in enumerate(other.calc_distances()):
                similarities.append(((i, j), len(sd.intersection(od))))
        return sorted(similarities, key=itemgetter(1), reverse=True)

    def rotate(self, axis: Axis):
        if not (self.min_point == -self.max_point):
            raise ValueError("Only real cubes centered at zero can be rotated")
        return Cube(
            (p.rotate(axis) for p in self.beacons),
            self.min_point,
            self.max_point,
        )

    def move(self, point: Point) -> Cube:
        return Cube(
            [p + point for p in self.beacons],
            self.min_point + point,
            self.max_point + point,
        )

    def orientations(self) -> Iterator[Cube]:
        yield (cube := self)
        for _ in range(3):
            yield (cube := cube.rotate(Axis.x))
        yield (cube := self.rotate(Axis.z))
        for _ in range(3):
            yield (cube := cube.rotate(Axis.y))
        yield (cube := self.rotate(Axis.z).rotate(Axis.z))
        for _ in range(3):
            yield (cube := cube.rotate(Axis.x))
        yield (cube := self.rotate(Axis.z).rotate(Axis.z).rotate(Axis.z))
        for _ in range(3):
            yield (cube := cube.rotate(Axis.y))
        yield (cube := self.rotate(Axis.y))
        for _ in range(3):
            yield (cube := cube.rotate(Axis.z))
        yield (cube := self.rotate(Axis.y).rotate(Axis.y).rotate(Axis.y))
        for _ in range(3):
            yield (cube := cube.rotate(Axis.z))

    def __repr__(self):
        return f"Cube(beacons={self.beacons}, min_point={self.min_point}, max_point{self.max_point})"


def read_puzzle_input(filename: str) -> list[list[Point]]:
    points: list[list[Point]] = []
    with open(filename) as stream:
        for block in stream.read().strip().split("\n\n"):
            points.append([])  # [scanner] = []
            for line in block.split("\n")[1:]:
                points[-1].append(Point(*parse("{:d},{:d},{:d}", line)))
    return points


def assemble_map(cubes: list[Cube]):
    map = [cubes[0]]
    found = [0]
    for ri, root_cube in enumerate(map):
        print(f"check Cube {found[ri]}:")
        for ci, cube in enumerate(cubes):
            if ci in found:
                continue
            print(f"... Cube {ci}")
            similarities = root_cube.similarity(cube)
            for (bid, obid), s in takewhile(lambda x: x[1] >= 12, similarities):
                # for (bid, obid), s in similarities:
                print(s)
                abort = False
                for c in cube.orientations():
                    ref_beacon = root_cube.beacons[bid]
                    moved_cube = c.move(ref_beacon - c.beacons[obid])
                    print(
                        f"fitting beacons: {root_cube.count_equal_beacons(moved_cube)}"
                    )
                    if (
                        root_cube.fits(moved_cube)
                        and root_cube.count_equal_beacons(moved_cube) >= 12
                    ):
                        # if root_cube.count_equal_beacons(moved_cube) >= 12:
                        map.append(moved_cube)
                        found.append(ci)
                        print(
                            f"found fit with {ci}, scanner @ {moved_cube.min_point + MAX_POINT}"
                        )
                        abort = True
                        break
                if abort:
                    break
    print(found)
    return map


def max_manhatten_distance(points: list[Point]) -> int:
    return max(p1.manhatten(p2) for p1 in points for p2 in points)


if __name__ == "__main__":
    inp = read_puzzle_input("input.txt")
    cubes: list[Cube] = [Cube(beacons=c) for c in inp]
    map: list[Cube] = assemble_map(cubes)
    beacons = set().union(*[set(c.beacons) for c in map])
    print(len(beacons))
    print(max_manhatten_distance([c.scanner for c in map]))
