from collections import namedtuple


Command = namedtuple("Command", "direction, units")


class Location(namedtuple("Location", "distance, depth")):
    def __add__(self, other):
        return Location(self.distance + other.distance, self.depth + other.depth)

    def __mul__(self, number):
        return Location(self.distance * number, self.depth * number)

    def __rmul__(self, number):
        return self * number


def line_to_command(line):
    command, units_string = line.strip().split()
    return Command(command, int(units_string))


def read_puzzle_input(filename):
    with open(filename) as stream:
        return [line_to_command(row) for row in stream.read().strip().split("\n")]


DIRECTION_DICT = {
    "forward": Location(1, 0),
    "up": Location(0, -1),
    "down": Location(0, 1),
}


def follow_course(course, starting_location=Location(0, 0)):
    location = starting_location
    for command in course:
        location += command.units * DIRECTION_DICT[command.direction]
    return location


if __name__ == "__main__":
    puzzle_input = read_puzzle_input("input.txt")
    target_location = follow_course(puzzle_input)
    print(target_location)
    print(target_location.distance * target_location.depth)
