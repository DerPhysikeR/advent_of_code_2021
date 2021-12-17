import pytest
from main import (
    read_bits,
    to_bin,
    sum_versions,
    eval_transmission,
    main,
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
        ("C0015000016115A2E0802F182340", 23),  # has an unnecessary hex at the end
        ("A0016C880162017C3686B18A3D4780", 31),
    ],
)
def test_sum_versions_with_actual_input(inp, sum_):
    assert sum_ == sum_versions(read_bits(to_bin(inp)))


@pytest.mark.parametrize(
    "inp, value",
    [
        ("C200B40A82", 3),
        ("04005AC33890", 54),
        ("880086C3E88112", 7),
        ("CE00C43D881120", 9),
        ("D8005AC2A8F0", 1),
        ("F600BC2D8F", 0),
        ("9C005AC2F8F0", 0),
        ("9C0141080250320F1802104A08", 1),
    ],
)
def test_eval_transmission(inp, value):
    assert value == eval_transmission(inp)


def test_main(capsys):
    main()
    captured = capsys.readouterr()
    assert captured.out == "843\n5390807940351\n"
