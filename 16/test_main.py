import pytest
from main import (
    read_bits,
    to_bin,
    sum_versions,
)


@pytest.mark.parametrize(
    "inp, sum_",
    [
        ([(1, [])], 1),
        ([(1, []), (1, [])], 2),
        ([(1, [(1, [])]), (1, [])], 3),
        ([(1, [(1, []), (1, [(1, [])])]), (1, [])], 5),
    ],
)
def test_sum_versions_with_basic_examples(inp, sum_):
    assert sum_ == sum_versions(inp)


@pytest.mark.parametrize(
    "inp, sum_",
    [
        ("D2FE28", 6),
        ("8A004A801A8002F478", 16),
        ("620080001611562C8802118E34", 12),
        ("C0015000016115A2E0802F182340", 23),
        ("A0016C880162017C3686B18A3D4780", 31),
    ],
)
def test_sum_versions_with_actual_input(inp, sum_):
    assert sum_ == sum_versions(read_bits(to_bin(inp)))
