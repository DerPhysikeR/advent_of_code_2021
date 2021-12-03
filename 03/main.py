def read_puzzle_input(filename):
    with open(filename) as stream:
        return stream.read().strip().split("\n")


def find_most_commnon_bits(binary_number_strings):
    digits = []
    for idx in range(len(binary_number_strings[0])):
        one_count = 0
        zero_count = 0
        for number in binary_number_strings:
            digit = number[idx]
            if digit == "1":
                one_count += 1
            elif digit == "0":
                zero_count += 1
            else:
                ValueError("{digit=} is not a binary digit")
        if one_count > zero_count:
            digits.append("1")
        elif one_count < zero_count:
            digits.append("0")
        else:
            ValueError("The number of zeros and ones is equal")
    return "".join(digits)


def decimal_from_binary_string(binary_string):
    return int(binary_string, 2)


def calc_gamma_and_epsilon_rate(puzzle_input):
    most_common_bits = find_most_commnon_bits(puzzle_input)
    inverted_bits = "".join("0" if bit == "1" else "1" for bit in most_common_bits)
    gamma_rate = decimal_from_binary_string(most_common_bits)
    epsilon_rate = decimal_from_binary_string(inverted_bits)
    return gamma_rate, epsilon_rate


if __name__ == "__main__":
    puzzle_input = read_puzzle_input("input.txt")
    gamma_rate, epsilon_rate = calc_gamma_and_epsilon_rate(puzzle_input)
    print(gamma_rate * epsilon_rate)
