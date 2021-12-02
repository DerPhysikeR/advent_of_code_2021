from main import (
    Command,
    Location,
    read_puzzle_input,
    follow_course,
    follow_course_correctly,
)


def test_location_add():
    assert Location(4, 6) == Location(1, 2) + Location(3, 4)


def test_location_mul():
    assert Location(2, 4) == Location(1, 2) * 2


def test_location_rmul():
    assert Location(2, 4) == 2 * Location(1, 2)


def test_read_puzzle_input():
    course = read_puzzle_input("test_input.txt")
    assert course[0] == Command("forward", 5)


def test_follow_course():
    course = read_puzzle_input("test_input.txt")
    assert Location(15, 10) == follow_course(course)


def test_follow_course_correctly():
    course = read_puzzle_input("test_input.txt")
    assert Location(15, 60) == follow_course_correctly(course)
