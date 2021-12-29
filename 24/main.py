from __future__ import annotations
from typing import Iterable, Iterator
from operator import methodcaller
from itertools import product


class ALU:
    def __init__(self):
        self.w: int
        self.x: int
        self.y: int
        self.z: int
        self.w = self.x = self.y = self.z = 0

    def exec_instructions(self, instructions: Iterable[str], inputs: Iterable[int]):
        inputs: Iterator[int] = iter(inputs)
        for instruction in instructions:
            if instruction.startswith("inp"):
                self.inp(instruction[-1], next(inputs))
            else:
                operator, a, b = instruction.split()
                try:
                    b = int(b)
                except ValueError:
                    pass
                methodcaller(operator, a, b)(self)
        return self

    def exec_program(self, program: str, inputs: Iterable[int]):
        instructions: list[str] = program.strip().split("\n")
        return self.exec_instructions(instructions, inputs)

    def inp(self, a, value):
        print(self.z)
        setattr(self, a, value)
        return self

    def add(self, a, b):
        if isinstance(b, int):
            setattr(self, a, getattr(self, a) + b)
        else:
            self.add(a, getattr(self, b))
        return self

    def mul(self, a, b):
        if isinstance(b, int):
            setattr(self, a, getattr(self, a) * b)
        else:
            self.mul(a, getattr(self, b))
        return self

    def div(self, a, b):
        if isinstance(b, int):
            setattr(self, a, getattr(self, a) // b)
        else:
            self.div(a, getattr(self, b))
        return self

    def mod(self, a, b):
        if isinstance(b, int):
            setattr(self, a, getattr(self, a) % b)
        else:
            self.mod(a, getattr(self, b))
        return self

    def eql(self, a, b):
        if isinstance(b, int):
            setattr(self, a, int(getattr(self, a) == b))
        else:
            self.eql(a, getattr(self, b))
        return self


def generate_model_number_lists():
    digits = list(reversed(range(1, 9 + 1)))
    yield from product(*[digits for _ in range(14)])


def find_largest_valid_model_number():
    with open("input.txt") as stream:
        MONAD = stream.read().strip()
    for model_number in generate_model_number_lists():
        print(model_number)
        # result = ALU().exec_program(MONAD, model_number).z
        result = is_valid_model_number(model_number)
        print(result)
        if result == 0:
            return model_number


def monad(w, i, j, k, z):
    x = int((z % 26) + j != w)
    y = (w + k) * x
    z = (z // i) * (25 * x + 1) + y
    return z


IJK = [
    (1, 11, 14),
    (1, 14, 6),
    (1, 15, 6),
    (1, 13, 13),
    (26, -12, 8),
    (1, 10, 8),
    (26, -15, 7),
    (1, 13, 10),
    (1, 10, 8),
    (26, -13, 12),
    (26, -13, 10),
    (26, -14, 8),
    (26, -2, 8),
    (26, -9, 7),
]


def is_valid_model_number(digits):
    z = 0
    for d, (i, j, k) in zip(digits, IJK):
        print(z)
        z = monad(d, i, j, k, z)
    return z


def to_digits(number: int) -> list[int]:
    return [int(d) for d in list(str(number))]


if __name__ == "__main__":
    """
    if z_n-1 mod 26 == w_n - j_n:
        z_n = z_n-1 div i_n
    else:
        z_n = 26 * (z_n-1 div i_n) + w_n + k_n

    In order to get to zero, each increasing step `z_n = 26 * (..) + w_n + k_n` has to
    be compensated by a decreasing step `z_n = z_n-1 div i_n`.
    `i_n == 1` signifies an increasing step and `i_n == 26` a decreasing step.
    `z_n-1 mod 26 == w_n - j_n` has to be fulfilled for a decreasing step to occur.
    Since `26 * (z_n-1 div i_n)` is always a multiple of 26, the modulus of the result
    of an increasing step is `w_n + k_n`.
    -> `w_n + k_n == z_n mod 26 == w_n+1 - j_n+1`
    -> `w_n+1 = w_n + k_n + j_n+1`
    This relation can be generalized to non-neighboring elements, if all increasing
    steps are compensated by decreasing steps inbetween.

    z_n+1 == z_n div i_n+1
    == z_n div 26
    == (26 * (z_n-1 div 1) + k_n + w_n) div 26
    == z_n-1 div 1
    == z_n-1

    -> w_n-1 + k_n-1 == w_n+2 - j_n+2
    -> w_n+2 = w_n-1 + k_n-1 + j_n+2

    Pairing up all increasing steps with their respective decreasing steps:
    1 -> 14
    2 -> 13
    3 -> 12
    4 -> 5
    6 -> 7
    8 -> 11
    9 -> 10

    leads to the following relations, when inserted in above equations:
    w_14 = w_1 + 5
    w_13 = w_2 + 4
    w_12 = w_3 - 8
    w_11 = w_8 - 3
    w_10 = w_9 - 5
    w_7 = w_6 - 7
    w_5 = w_4 + 1

    Choosing the larger digit of each pair to be 9 leads to the solution for the first part:
    45989929946199
    Choosing the smaller digit of each pair to be 1 leads to the solution for the second part:
    11912814611156
    """

    # solution calculated on paper
    is_valid_model_number(to_digits(45989929946199))
    print(45989929946199)
    # solution calculated on paper
    is_valid_model_number(to_digits(11912814611156))
    print(11912814611156)
