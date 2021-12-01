def read_puzzle_input(filename):
    with open(filename) as stream:
        return [int(row) for row in stream.read().strip().split("\n")]


def count_depth_increases(depth_measurements):
    previous_depth = depth_measurements[0]
    depth_increases_counter = 0
    for depth in depth_measurements:
        if depth > previous_depth:
            depth_increases_counter += 1
        previous_depth = depth
    return depth_increases_counter


if __name__ == "__main__":
    puzzle_input = read_puzzle_input("input.txt")
    print(count_depth_increases(puzzle_input))
