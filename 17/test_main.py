import pytest
from main import (
    Vector,
    Probe,
    TargetArea,
)

TEST_INPUT = "target area: x=20..30, y=-10..-5"


@pytest.fixture
def target_area():
    return TargetArea.from_input(TEST_INPUT)


@pytest.mark.parametrize(
    "probe, hits",
    [
        (Probe(Vector(0, 0), Vector(-7, 2)), False),
        (Probe(Vector(0, 0), Vector(7, 2)), True),
        (Probe(Vector(0, 0), Vector(6, 3)), True),
        (Probe(Vector(0, 0), Vector(9, 0)), True),
        (Probe(Vector(0, 0), Vector(17, -4)), False),
    ],
)
def test_probe_hits_target(probe, hits, target_area):
    assert hits == probe.hits(target_area)


@pytest.mark.parametrize(
    "probe, maxy",
    [
        (Probe(Vector(0, 0), Vector(6, 9)), 45),
    ],
)
def test_probe_maxy(probe, maxy):
    assert maxy == probe.maxy()


# def test_find_max_y(target_area):
#     assert 45 == find_max_y(target_area)
