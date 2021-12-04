from main import (
    BingoBoard,
    read_puzzle_input,
    find_first_winning_board_score,
    find_last_winning_board_score,
)


def test_bingo_board_wins_by_row():
    bb = BingoBoard([[1, 2], [3, 4]])
    assert bb.call_number(1) == False
    assert bb.call_number(2) == True


def test_bingo_board_wins_by_col():
    bb = BingoBoard([[1, 2], [3, 4]])
    assert bb.call_number(1) == False
    assert bb.call_number(3) == True


def test_read_puzzle_input():
    called_numbers, boards = read_puzzle_input("test_input.txt")
    assert called_numbers[0] == 7
    assert called_numbers[-1] == 1
    assert boards[0][0][0] == 22
    assert boards[-1][-1][-1] == 7


def test_find_first_winning_board_score():
    assert 4512 == find_first_winning_board_score(*read_puzzle_input("test_input.txt"))


def test_find_last_winning_board_score():
    assert 1924 == find_last_winning_board_score(*read_puzzle_input("test_input.txt"))
