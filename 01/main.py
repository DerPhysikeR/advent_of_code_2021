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


def sum_sliding_window(values, window_length):
    sums = []
    for i, value in enumerate(values):
        try:
            sums.append(sum(values[i:i+3]))
        except IndexError:
            pass
    return sums


def count_windowed_depth_increases(depth_measurements, window_length=3):
    sliding_window_sums = sum_sliding_window(
        depth_measurements, window_length=window_length
    )
    return count_depth_increases(sliding_window_sums)


if __name__ == "__main__":
    puzzle_input = read_puzzle_input("input.txt")
    print(count_depth_increases(puzzle_input))
    print(count_windowed_depth_increases(puzzle_input))
