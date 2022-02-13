use std::fs;

#[derive(Debug)]
struct BingoBoard {
    numbers: Vec<Vec<u32>>,
}

impl BingoBoard {
    fn from_string(string: &str) -> Self {
        let mut board: Vec<Vec<u32>> = Vec::new();
        for line in string.lines() {
            board.push(Vec::new());
            for num_string in line.split_whitespace() {
                let l = board.len();
                board[l - 1].push(num_string.parse().unwrap());
            }
        }
        assert!(board.iter().all(|v| v.len() == board.len()));
        Self { numbers: board }
    }

    fn check_rows_won(&self, called_numbers: &[u32]) -> bool {
        self.numbers
            .iter()
            .any(|x| x.iter().all(|&y| called_numbers.contains(&y)))
    }

    fn check_cols_won(&self, called_numbers: &[u32]) -> bool {
        (0..self.numbers[0].len())
            .any(|i| self.numbers.iter().all(|v| called_numbers.contains(&v[i])))
    }

    fn has_won(&self, called_numbers: &[u32]) -> bool {
        self.check_rows_won(called_numbers) || self.check_cols_won(called_numbers)
    }

    fn sum_unmarked_numbers(&self, called_numbers: &[u32]) -> u32 {
        self.numbers
            .iter()
            .flatten()
            .filter(|n| !called_numbers.contains(n))
            .sum()
    }

    fn calc_score(&self, called_numbers: &[u32]) -> u32 {
        self.sum_unmarked_numbers(called_numbers) * called_numbers[called_numbers.len() - 1]
    }
}

fn find_first_winning_board_score(
    numbers_to_call: &[u32],
    bingo_boards: &[BingoBoard],
) -> Option<u32> {
    for upper in 1..numbers_to_call.len() {
        let called_numbers = &numbers_to_call[..upper];
        for bingo_board in bingo_boards {
            if bingo_board.has_won(called_numbers) {
                return Some(bingo_board.calc_score(called_numbers));
            }
        }
    }
    None
}

fn find_last_winning_board_score(
    numbers_to_call: &[u32],
    bingo_boards: &[BingoBoard],
) -> Option<u32> {
    let mut last_score = None;
    let mut bingo_boards: Vec<&BingoBoard> = bingo_boards.iter().collect();
    for upper in 1..numbers_to_call.len() {
        let called_numbers = &numbers_to_call[..upper];
        let mut bingo_boards_in_progress: Vec<&BingoBoard> = Vec::new();
        for bingo_board in bingo_boards {
            if bingo_board.has_won(called_numbers) {
                last_score = Some(bingo_board.calc_score(called_numbers));
            } else {
                bingo_boards_in_progress.push(bingo_board);
            }
        }
        bingo_boards = bingo_boards_in_progress;
    }
    last_score
}

fn read_puzzle_input(filename: &str) -> (Vec<u32>, Vec<BingoBoard>) {
    let content = fs::read_to_string(filename).expect("Couln't read input file");
    let blocks: Vec<&str> = content.split("\n\n").collect();
    assert!(blocks.len() >= 2);
    let numbers_to_call = blocks[0].split(",").map(|x| x.parse().unwrap()).collect();
    let bingo_boards = blocks[1..]
        .iter()
        .map(|&b| BingoBoard::from_string(b))
        .collect();
    (numbers_to_call, bingo_boards)
}

fn main() {
    let (numbers_to_call, bingo_boards) = read_puzzle_input("../input.txt");
    println!(
        "{:?}",
        find_first_winning_board_score(&numbers_to_call, &bingo_boards).unwrap()
    );
    println!(
        "{:?}",
        find_last_winning_board_score(&numbers_to_call, &bingo_boards).unwrap()
    );
}
