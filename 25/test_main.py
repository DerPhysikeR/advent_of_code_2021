import pytest
from main import (
    Vector,
    Seafloor,
)

TEST_INPUT = "test_input.txt"


@pytest.mark.parametrize(
    "vec, maxvec, result",
    [
        (Vector(10, 10), Vector(10, 10), Vector(0, 0)),
        (Vector(10, 10), Vector(9, 9), Vector(1, 1)),
        (Vector(10, 10), Vector(9, 8), Vector(1, 2)),
    ],
)
def test_vector__mod__(vec, maxvec, result):
    assert result == vec % maxvec


def test_seafloor_evolve_until_gridlock():
    sf = Seafloor.from_file(TEST_INPUT)
    assert 58 == Seafloor.from_file(TEST_INPUT).evolve_until_gridlock()
