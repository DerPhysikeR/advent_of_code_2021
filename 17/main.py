from __future__ import annotations
from typing import Iterator, NamedTuple, Optional
from itertools import count
from parse import parse


class Vector(NamedTuple):
    x: int
    y: int

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)


class TargetArea(NamedTuple):
    min: Vector
    max: Vector

    def __contains__(self, v: Vector) -> bool:
        return self.inx(v.x) and self.iny(v.y)

    def inx(self, x: int):
        return self.min.x <= x <= self.max.x

    def iny(self, y: int):
        return self.min.y <= y <= self.max.y

    @classmethod
    def from_input(cls, inp: str) -> TargetArea:
        p = parse("target area: x={minx:d}..{maxx:d}, y={miny:d}..{maxy:d}", inp).named
        return cls(Vector(p["minx"], p["miny"]), Vector(p["maxx"], p["maxy"]))


class Probe(NamedTuple):
    pos: Vector
    vel: Vector

    def launch(self) -> Iterator[Probe]:
        pos: Vector = self.pos
        vel: Vector = self.vel
        while True:
            yield Probe(pos, vel)
            pos += vel
            vel = Vector(self.update_vx(vel.x), vel.y - 1)

    @staticmethod
    def update_vx(v: int) -> int:
        if v > 0:
            return v - 1
        if v < 0:
            return v + 1
        return 0

    def hits(self, target_area: TargetArea) -> bool:
        for probe in self.launch():
            if probe.pos in target_area:
                return True
            if probe.pos.y < target_area.min.y:
                break
        return False

    def trajectory(self, target_area: TargetArea) -> Iterator[Probe]:
        for probe in self.launch():
            yield probe
            if probe.pos in target_area:
                return True
            if probe.pos.y < target_area.min.y:
                return False

    def maxy(self) -> int:
        y = self.pos.y
        for probe in self.launch():
            if y > probe.pos.y:
                break
            y = probe.pos.y
        return y


def canvas(target_area: TargetArea, probe: Optional[Probe]):
    if probe:
        probe_positions: set[Vector] = set(p.pos for p in probe.trajectory(target_area))
    else:
        probe_positions: set[Vector] = set()
    minx = min(target_area.min.x, min(p.x for p in probe_positions))
    maxx = max(target_area.max.x, max(p.x for p in probe_positions))
    miny = min(target_area.min.y, min(p.y for p in probe_positions))
    maxy = max(target_area.max.y, max(p.y for p in probe_positions))
    canv = []
    for y in range(maxy, miny - 1, -1):
        canv.append([])
        for x in range(minx, maxx + 1):
            vec = Vector(x, y)
            if vec in probe_positions:
                canv[-1].append("#")
            elif vec in target_area:
                canv[-1].append("T")
            else:
                canv[-1].append(".")
    return "\n".join("".join(row) for row in canv)


if __name__ == "__main__":
    INPUT = "target area: x=211..232, y=-124..-69"
    target_area: TargetArea = TargetArea.from_input(INPUT)

    # print(canvas(target_area, Probe(Vector(0, 0), Vector(21, 300))))

    # for vy in count(100):
    #     if Probe(Vector(0, 0), Vector(21, vy)).hits(target_area):
    #         print(vy)

    print(Probe(Vector(0, 0), Vector(21, 123)).maxy())
