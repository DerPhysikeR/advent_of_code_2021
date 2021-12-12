import pytest
from main_as_class import (
    Graph,
)


@pytest.mark.parametrize(
    "test_input, n_paths",
    [
        ("small_test_input.txt", 10),
        ("medium_test_input.txt", 19),
        ("large_test_input.txt", 226),
    ],
)
def test_calc_n_paths_with_test_inputs(test_input, n_paths):
    assert n_paths == Graph.from_file(test_input).calc_n_paths()


@pytest.mark.parametrize(
    "test_input, n_paths",
    [
        ("small_test_input.txt", 36),
        ("medium_test_input.txt", 103),
        ("large_test_input.txt", 3509),
    ],
)
def test_calc_n_paths_revisited_with_test_inputs(test_input, n_paths):
    assert n_paths == Graph.from_file(test_input).calc_n_paths_revisited()
