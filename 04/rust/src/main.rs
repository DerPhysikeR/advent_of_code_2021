use std::fs;

#[derive(Debug)]
struct BingoBoard {
    numbers: Vec<Vec<u32>>,
    hit_numbers: Vec<u32>,
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
        Self {
            numbers: board,
            hit_numbers: Vec::new(),
        }
    }

    fn contains(&self, number: u32) -> bool {
        self.numbers.iter().any(|v| v.contains(&number))
    }

    fn call_number(&mut self, number: u32) {
        if self.hit_numbers.contains(&number) || self.has_won() {
            return;
        };
        if self.contains(number) {
            self.hit_numbers.push(number);
        }
    }

    fn check_rows_won(&self) -> bool {
        self.numbers
            .iter()
            .any(|x| x.iter().all(|&y| self.hit_numbers.contains(&y)))
    }

    fn check_cols_won(&self) -> bool {
        (0..self.numbers[0].len()).any(|i| {
            self.numbers
                .iter()
                .all(|v| self.hit_numbers.contains(&v[i]))
        })
    }

    fn has_won(&self) -> bool {
        self.check_rows_won() || self.check_cols_won()
    }

    fn calc_score(&self) -> u32 {
        if !self.has_won() {
            return 0;
        }
        let unmarked_sum: u32 = self
            .numbers
            .iter()
            .flatten()
            .filter(|x| !self.hit_numbers.contains(x))
            .sum();
        unmarked_sum * self.hit_numbers[self.hit_numbers.len() - 1]
    }
}

fn find_first_winning_board<'a>(
    numbers_to_call: &Vec<u32>,
    bingo_boards: &'a mut Vec<BingoBoard>,
) -> Option<&'a BingoBoard> {
    for number in numbers_to_call {
        for i in 0..bingo_boards.len() {
            bingo_boards[i].call_number(*number);
            if bingo_boards[i].has_won() {
                return Some(&bingo_boards[i]);
            }
        }
        //     for bingo_board in bingo_boards {
        //         bingo_board.call_number(*number);
        //         if bingo_board.has_won() {
        //             return Some(bingo_board);
        //         };
        //     }
    }
    None
}

fn find_last_winning_board<'a>(
    numbers_to_call: &Vec<u32>,
    bingo_boards: &'a mut Vec<BingoBoard>,
) -> u32 {
    // let boards_to_filter = bingo_boards.iter().collect();
    // let mut bingo_boards: Vec<&BingoBoard> = bingo_boards.iter().filter(|bb| !bb.has_won()).collect();
    let mut last_won_score = 0;
    for number in numbers_to_call {
        for i in 0..bingo_boards.len() {
            if bingo_boards[i].has_won() {
                continue;
            }
            bingo_boards[i].call_number(*number);
            last_won_score = bingo_boards[i].calc_score();
        }
    }
    last_won_score
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
    let (numbers_to_call, mut bingo_boards) = read_puzzle_input("../input.txt");
    let first_winning_board =
        find_first_winning_board(&numbers_to_call, &mut bingo_boards).unwrap();
    println!("{:?}", first_winning_board.calc_score());
    let last_winning_score = find_last_winning_board(&numbers_to_call, &mut bingo_boards);
    println!("{:?}", last_winning_score);
}
