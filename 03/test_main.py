from main import (
    read_puzzle_input,
    find_most_commnon_bits,
    calc_gamma_and_epsilon_rate,
)


def test_find_most_common_bits():
    puzzle_input = read_puzzle_input("test_input.txt")
    assert "10110" == find_most_commnon_bits(puzzle_input)


def test_calc_gamma_rate():
    puzzle_input = read_puzzle_input("test_input.txt")
    assert (22, 9) == calc_gamma_and_epsilon_rate(puzzle_input)
