from __future__ import annotations
from collections import defaultdict
from itertools import pairwise


class Polymer:
    def __init__(self, template: str, rules: dict[str, str]):
        self.poly = list(template)
        self.links = list(range(1, len(template))) + [0]
        self.rules = rules
        self.start = 0

    def evolve(self, n):
        for _ in range(n):
            self._evolve()
        return self

    def _evolve(self):
        idx1 = self.start
        idx2 = self.links[idx1]
        while True:
            if (key := "".join([self.poly[idx1], self.poly[idx2]])) in self.rules:
                self.poly.append(self.rules[key])
                self.links.append(idx2)
                self.links[idx1] = len(self.poly) - 1
            idx1, idx2 = idx2, self.links[idx2]
            if idx2 == self.start:
                break

    def mc_minus_lc_element_count(self):
        element_counts = defaultdict(int)
        for element in self.poly:
            element_counts[element] += 1
        counts = sorted(list(element_counts.values()))
        return counts[-1] - counts[0]

    def __iter__(self):
        idx = self.start
        while True:
            yield self.poly[idx]
            idx = self.links[idx]
            if idx == self.start:
                break

    def __str__(self):
        return "".join(letter for letter in self)


class FastPolymer:
    def __init__(self, template: str, rules: dict[str, str]):
        self.rules = rules
        self.element_counts = defaultdict(
            int, {k: template.count(k) for k in set(template)}
        )
        self.pairing_counts = defaultdict(
            int,
            {
                "".join(pair): template.count("".join(pair))
                for pair in pairwise(template)
            },
        )

    def evolve(self, n: int) -> FastPolymer:
        for _ in range(n):
            self._evolve()
        return self

    def _evolve(self):
        new_pairing_counts = self.pairing_counts.copy()
        for key, insertion in self.rules.items():
            if key in self.pairing_counts:
                count = self.pairing_counts[key]
                self.element_counts[insertion] += count
                new_pairing_counts[key] -= count
                pair1, pair2 = key[0] + insertion, insertion + key[1]
                new_pairing_counts[pair1] += count
                new_pairing_counts[pair2] += count
        self.pairing_counts = new_pairing_counts

    def mc_minus_lc_element_count(self):
        counts = sorted(list(self.element_counts.values()))
        return counts[-1] - counts[0]


def read_puzzle_input(filename: str) -> tuple[str, dict[str, str]]:
    with open(filename) as stream:
        poly_template = stream.readline().strip()
        stream.readline()
        rules = {line[:2]: line[-1] for line in stream.read().strip().split("\n")}
    return poly_template, rules


if __name__ == "__main__":
    polymer = Polymer(*read_puzzle_input("input.txt"))
    print(polymer.evolve(10).mc_minus_lc_element_count())

    fast_polymer = FastPolymer(*read_puzzle_input("input.txt"))
    print(fast_polymer.evolve(40).mc_minus_lc_element_count())
