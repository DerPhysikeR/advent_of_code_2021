import pytest
from main import (
    read_puzzle_input,
    count_1478_in_output,
    get_output_val_sum,
)


@pytest.fixture
def entries():
    return read_puzzle_input("test_input.txt")


@pytest.fixture
def more_entries():
    return read_puzzle_input("test_larger_input.txt")


def test_count_1478_in_output(more_entries):
    assert 26 == count_1478_in_output(more_entries)


def test_get_output_val_sum_with_test_input(entries):
    assert 5353 == get_output_val_sum(entries)


def test_get_output_val_sum_with_test_larger_input(more_entries):
    assert 61229 == get_output_val_sum(more_entries)
