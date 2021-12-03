def read_puzzle_input(filename):
    with open(filename) as stream:
        return stream.read().strip().split("\n")


def find_most_common_bit(bits, fill_value=None):
    assert set(bits) == set(["0", "1"])
    one_count = bits.count("1")
    zero_count = bits.count("0")
    if one_count > zero_count:
        return "1"
    elif one_count < zero_count:
        return "0"
    else:
        if fill_value is None:
            raise ValueError("The number of zeros and ones is equal")
        else:
            return fill_value


def find_most_commnon_bits(binary_number_strings):
    digits = []
    for idx in range(len(binary_number_strings[0])):
        digits.append(find_most_common_bit([n[idx] for n in binary_number_strings]))
    return "".join(digits)


def decimal_from_binary_string(binary_string):
    return int(binary_string, 2)


def calc_gamma_and_epsilon_rate(puzzle_input):
    most_common_bits = find_most_commnon_bits(puzzle_input)
    inverted_bits = "".join("0" if bit == "1" else "1" for bit in most_common_bits)
    gamma_rate = decimal_from_binary_string(most_common_bits)
    epsilon_rate = decimal_from_binary_string(inverted_bits)
    return gamma_rate, epsilon_rate


def filter_numbers(puzzle_input, filter_function):
    numbers = puzzle_input[:]
    for idx in range(len(numbers[0])):
        numbers = filter_function(numbers, idx)
        if len(numbers) == 1:
            break
    else:
        raise ValueError("There is never a single number left ...")
    return decimal_from_binary_string(numbers[0])


def ogr_filter_function(numbers, idx):
    mcb = find_most_common_bit([n[idx] for n in numbers], "1")
    return [n for n in numbers if n[idx] == mcb]


def calc_oxygen_generator_rating(puzzle_input):
    return filter_numbers(puzzle_input, ogr_filter_function)


def csr_filter_function(numbers, idx):
    mcb = find_most_common_bit([n[idx] for n in numbers], "1")
    return [n for n in numbers if n[idx] != mcb]


def calc_co2_scrubber_rating(puzzle_input):
    return filter_numbers(puzzle_input, csr_filter_function)


if __name__ == "__main__":
    puzzle_input = read_puzzle_input("input.txt")
    gamma_rate, epsilon_rate = calc_gamma_and_epsilon_rate(puzzle_input)
    print(gamma_rate * epsilon_rate)

    oxygen_generator_rating = calc_oxygen_generator_rating(puzzle_input)
    co2_scrubber_rating = calc_co2_scrubber_rating(puzzle_input)
    print(oxygen_generator_rating * co2_scrubber_rating)
