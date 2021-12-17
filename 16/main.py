from __future__ import annotations
from typing import Iterator, NamedTuple, Iterable
from more_itertools import take
from math import prod


HEX_TO_BIN = {
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


def to_bin(hex: str):
    return "".join(HEX_TO_BIN[h] for h in hex)


class OutOfElementsError(Exception):
    pass


def read_bits(bits: Iterable[str]):
    bits = iter(bits)
    while True:
        try:
            header: str = safe_take(6, bits)
        except OutOfElementsError:
            break
        version: int = bin_to_int(header[:3])
        type_id: int = bin_to_int(header[3:])
        if type_id == 4:
            yield (version, type_id, parse_type_4(bits))
        else:
            try:
                lenght_type_id: int = bin_to_int(safe_take(1, bits))
            except OutOfElementsError:
                break
            if lenght_type_id == 0:
                try:
                    bit_length: int = bin_to_int(safe_take(15, bits))
                except OutOfElementsError:
                    break
                yield (
                    version,
                    type_id,
                    lenght_type_id,
                    list(read_bits(safe_take(bit_length, bits))),
                )
            else:
                n_packets: int = bin_to_int(safe_take(11, bits))
                try:
                    yield (
                        version,
                        type_id,
                        lenght_type_id,
                        list(take(n_packets, read_bits(bits))),
                    )
                except OutOfElementsError:
                    return


def parse_type_4(bits):
    groups = []
    while (group := safe_take(5, bits))[0] == "1":
        groups.append(group[1:])
    groups.append(group[1:])
    value_str = "".join(groups)
    value: int = bin_to_int(value_str)
    return value


def safe_take(n: int, it: Iterator) -> str:
    taken = take(n, it)
    if len(taken) < n:
        raise OutOfElementsError
    return "".join(taken)


def bin_to_int(bin: str) -> int:
    return int(bin, 2)


def sum_versions(packets):
    sum_ = 0
    for p in packets:
        sum_ += p[0]
        try:
            sum_ += sum_versions(p[-1])
        except TypeError:
            pass
    return sum_


def evalp(packet):
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
        return 1 if evalp(packet[-1][0]) > evalp(packet[-1][1]) else 0
    if packet[1] == 6:
        return 1 if evalp(packet[-1][0]) < evalp(packet[-1][1]) else 0
    if packet[1] == 7:
        return 1 if evalp(packet[-1][0]) == evalp(packet[-1][1]) else 0



def eval_transmission(transmission):
    tree = list(read_bits(to_bin(transmission)))[0]
    print(tree)
    return evalp(tree)


if __name__ == "__main__":
    puzzle_input = read_puzzle_input("input.txt")
    packet_tree = list(read_bits(to_bin(puzzle_input)))
    print(sum_versions(packet_tree))
    print(evalp(packet_tree[0]))
