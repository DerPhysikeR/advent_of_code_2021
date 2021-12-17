from __future__ import annotations
from typing import Iterator, Iterable
from more_itertools import take
from math import prod


HEX_TO_BIN: dict[str, str] = {
    "0": "0000",
    "1": "0001",
    "2": "0010",
    "3": "0011",
    "4": "0100",
    "5": "0101",
    "6": "0110",
    "7": "0111",
    "8": "1000",
    "9": "1001",
    "A": "1010",
    "B": "1011",
    "C": "1100",
    "D": "1101",
    "E": "1110",
    "F": "1111",
}


def read_puzzle_input(filename: str) -> str:
    with open(filename) as stream:
        return stream.read().strip()


def to_bin(hex: str) -> str:
    return "".join(HEX_TO_BIN[h] for h in hex)


class OutOfElementsError(Exception):
    pass


def read_bits(bit_string: Iterable[str]):
    bits: Iterator[str] = iter(bit_string)
    while True:
        try:
            version: int
            type_id: int
            version, type_id = read_header(bits)
        except OutOfElementsError:
            break
        if type_id == 4:
            try:
                yield (version, type_id, parse_type_4(bits))
            except OutOfElementsError:
                break
        else:
            try:
                lenght_type_id: int = bin_to_int(safe_take(1, bits))
            except OutOfElementsError:
                break
            if lenght_type_id == 0:
                try:
                    bit_length: int = bin_to_int(safe_take(15, bits))
                    bits_to_read: str = safe_take(bit_length, bits)
                except OutOfElementsError:
                    break
                yield (
                    version,
                    type_id,
                    lenght_type_id,
                    list(read_bits(bits_to_read)),
                )
            else:
                try:
                    n_packets: int = bin_to_int(safe_take(11, bits))
                except OutOfElementsError:
                    break
                yield (
                    version,
                    type_id,
                    lenght_type_id,
                    list(take(n_packets, read_bits(bits))),
                )


def read_header(bits: Iterator[str]) -> tuple[int, int]:
    version: int = bin_to_int(safe_take(3, bits))
    type_id: int = bin_to_int(safe_take(3, bits))
    return version, type_id


def parse_type_4(bits: Iterator[str]) -> int:
    groups: list[str] = []
    while (group := safe_take(5, bits))[0] == "1":
        groups.append(group[1:])
    groups.append(group[1:])
    return bin_to_int("".join(groups))


def safe_take(n: int, it: Iterator[str]) -> str:
    taken: list[str] = take(n, it)
    if len(taken) < n:
        raise OutOfElementsError
    return "".join(taken)


def bin_to_int(bin: str) -> int:
    return int(bin, 2)


def sum_versions(packets) -> int:
    sum_: int = 0
    for p in packets:
        sum_ += p[0]
        try:
            sum_ += sum_versions(p[-1])
        except TypeError:
            pass
    return sum_


def evalp(packet) -> int:
    if packet[1] == 4:
        return packet[-1]
    if packet[1] == 0:
        return sum(evalp(p) for p in packet[-1])
    if packet[1] == 1:
        return prod(evalp(p) for p in packet[-1])
    if packet[1] == 2:
        return min(evalp(p) for p in packet[-1])
    if packet[1] == 3:
        return max(evalp(p) for p in packet[-1])
    if packet[1] == 5:
        return evalp(packet[-1][0]) > evalp(packet[-1][1])
    if packet[1] == 6:
        return evalp(packet[-1][0]) < evalp(packet[-1][1])
    if packet[1] == 7:
        return evalp(packet[-1][0]) == evalp(packet[-1][1])
    raise ValueError("Invalid type ID {packet[1]}")


def eval_transmission(transmission: str) -> int:
    tree = list(read_bits(to_bin(transmission)))[0]
    return evalp(tree)


def main():
    puzzle_input: str = read_puzzle_input("input.txt")
    packet_tree = list(read_bits(to_bin(puzzle_input)))
    print(sum_versions(packet_tree))
    print(evalp(packet_tree[0]))


if __name__ == "__main__":
    main()
