import pytest
from main import (
    read_puzzle_input,
    count_1478_in_output,
)


@pytest.fixture
def entries():
    return read_puzzle_input("test_input.txt")


@pytest.fixture
def more_entries():
    return read_puzzle_input("test_larger_input.txt")


def test_count_1478_in_output(more_entries):
    assert 26 == count_1478_in_output(more_entries)
