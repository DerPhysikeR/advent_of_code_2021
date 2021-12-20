from __future__ import annotations
from typing import Iterator, NamedTuple, Optional, Iterable
from enum import Enum
from itertools import product


class Color(Enum):
    dark = "."
    light = "#"


class Point(NamedTuple):
    row: int
    col: int

    def neighbors(self):
        d = (-1, 0, 1)
        for dr, dc in product(d, d):
            yield Point(self.row + dr, self.col + dc)


Index = tuple[Color, Color, Color, Color, Color, Color, Color, Color, Color]


class Algo:
    def __init__(self, light_pixels: set[int]):
        self.light_pixels = light_pixels

    def convert_to_int(self, index: Index):
        return int("".join("1" if c == Color.light else "0" for c in index), 2)

    def by_color(self, index: Index):
        return self[int("".join("1" if c == Color.light else "0" for c in index), 2)]

    def __getitem__(self, index: int):
        if not (0 <= index <= 511):
            raise IndexError("Algo index out of range")
        return Color.light if index in self.light_pixels else Color.dark

    @classmethod
    def from_string(cls, algo: str) -> Algo:
        return Algo(set(i for i, p in enumerate(algo) if p == Color.light.value))


class Image:
    def __init__(self, pixels: set[Point], background: Color = Color.dark):
        self.pixels: set[Point] = pixels
        self.background: Color = background

    @property
    def foreground(self):
        return Color.light if self.background == Color.dark else Color.dark

    def enhance(self, algo: Algo) -> Image:
        new_background = algo[0] if self.background == Color.dark else algo[511]
        new_pixels: set[Point] = set()
        min_point, max_point = self.find_min_max()
        for ri in range(min_point.row - 1, max_point.row + 2):
            for ci in range(min_point.col - 1, max_point.col + 2):
                point = Point(ri, ci)
                index = tuple(self[p] for p in point.neighbors())
                color = algo.by_color(index)
                if color != new_background:
                    new_pixels.add(point)
        return Image(new_pixels, new_background)

    def find_min_max(self) -> tuple[Point, Point]:
        minrow = min(p.row for p in self.pixels)
        maxrow = max(p.row for p in self.pixels)
        mincol = min(p.col for p in self.pixels)
        maxcol = max(p.col for p in self.pixels)
        return Point(minrow, mincol), Point(maxrow, maxcol)

    def __str__(self) -> str:
        min_point: Point
        max_point: Point
        min_point, max_point = self.find_min_max()
        image: list[list[str]] = []
        for ri in range(min_point.row, max_point.row + 1):
            image.append([])
            for ci in range(min_point.col, max_point.col + 1):
                image[-1].append(
                    self.foreground.value
                    if Point(ri, ci) in self.pixels
                    else self.background.value
                )
        return "\n".join("".join(row) for row in image)

    @classmethod
    def from_string(cls, image: str, background: Color) -> Image:
        pixels: set[Point] = set()
        for ri, row in enumerate(image.split("\n")):
            for ci, c in enumerate(row):
                if c != background.value:
                    pixels.add(Point(ri, ci))
        return cls(pixels, background)

    def __len__(self):
        return len(self.pixels)

    def __getitem__(self, point: Point):
        return self.foreground if point in self.pixels else self.background


def count_light_pixels_after_enhancements(
    image: Image, algo: Algo, n_enhancements: int
) -> int:
    for _ in range(n_enhancements):
        image = image.enhance(algo)
    return len(image)


def read_puzzle_input(filename: str) -> tuple[Image, Algo]:
    with open(filename) as stream:
        algo = Algo.from_string(stream.readline().strip())
        stream.readline()
        image = Image.from_string(stream.read().strip(), Color.dark)
    return image, algo


if __name__ == "__main__":
    image: Image
    algo: Algo
    image, algo = read_puzzle_input("input.txt")
    print(count_light_pixels_after_enhancements(image, algo, 2))
