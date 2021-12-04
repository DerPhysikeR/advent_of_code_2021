class BingoBoard:
    def __init__(self, numbers):
        self.numbers = numbers
        self._number_set = set().union(*self.numbers)
        self._hit_numbers = set()
        self._row_sets = [set(row) for row in self.numbers]
        self._col_sets = [set(col) for col in zip(*self.numbers)]

    def __getitem__(self, key):
        return self.numbers[key]

    def _check_if_won(self):
        for rs in self._row_sets:
            if rs.issubset(self._hit_numbers):
                return True
        for cs in self._col_sets:
            if cs.issubset(self._hit_numbers):
                return True
        return False

    def call_number(self, number):
        if number in self._number_set:
            self._hit_numbers.add(number)
        return self._check_if_won()

    def __repr__(self):
        return f"BingoBoard({self.numbers})"

    def score(self):
        return sum(self._number_set.difference(self._hit_numbers))


def read_puzzle_input(filename):
    with open(filename) as stream:
        called_numbers = [int(n) for n in stream.readline().strip().split(",")]
        bingo_board_strings = stream.read().strip().split("\n\n")
        bingo_board_numbers = [
            [[int(n) for n in row.split()] for row in bbs.split("\n")]
            for bbs in bingo_board_strings
        ]
    return called_numbers, [BingoBoard(bb) for bb in bingo_board_numbers]


def find_first_winning_board_score(numbers_to_call, bingo_boards):
    for number in numbers_to_call:
        for i, bingo_board in enumerate(bingo_boards):
            if bingo_board.call_number(number):
                return number * bingo_board.score()


def find_last_winning_board_score(numbers_to_call, bingo_boards):
    for number in numbers_to_call:
        filtered_bingo_boards = []
        for bingo_board in bingo_boards:
            if not bingo_board.call_number(number):
                filtered_bingo_boards.append(bingo_board)
        bingo_boards = filtered_bingo_boards
        if len(bingo_boards) == 1:
            last_board = bingo_boards[0]
        if len(bingo_boards) == 0:
            return number * last_board.score()


if __name__ == "__main__":
    print(find_first_winning_board_score(*read_puzzle_input("input.txt")))
    print(find_last_winning_board_score(*read_puzzle_input("input.txt")))
