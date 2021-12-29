import pytest
from textwrap import dedent
from main import (
    ALU,
)


def test_alu_with_short_negating_program():
    assert ALU().inp("x", 5).mul("x", -1).x == -5


def test_alu_with_comparison_if_z_is_3_times_x_with_correct_input():
    assert ALU().inp("z", 3).inp("x", 9).mul("z", 3).eql("z", "x").z == 1


def test_alu_with_comparison_if_z_is_3_times_x_with_incorrect_input():
    assert ALU().inp("z", 3).inp("x", 8).mul("z", 3).eql("z", "x").z == 0


TO_BINARY = dedent(
    """\
    inp w
    add z w
    mod z 2
    div w 2
    add y w
    mod y 2
    div w 2
    add x w
    mod x 2
    div w 2
    mod w 2"""
)


@pytest.mark.parametrize("number", range(16))
def test_alu_with_conversion_program_to_binary(number):
    alu = ALU().exec_program(TO_BINARY, [number])
    assert alu.z + 2 * alu.y + 4 * alu.x + 8 * alu.w == number
