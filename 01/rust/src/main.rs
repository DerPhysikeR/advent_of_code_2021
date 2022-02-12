use std::fs;

fn read_puzzle_input(filename: &str) -> Vec<i32> {
    let content = fs::read_to_string(filename).expect("Couldn't read file!");
    content.lines().map(|x| x.parse().unwrap()).collect()
}

fn count_depth_increases(depths: &Vec<i32>) -> i32 {
    assert!(depths.len() >= 1);
    let mut count_increases = 0;
    for i in 1..depths.len() {
        if depths[i] > depths[i - 1] {
            count_increases += 1;
        }
    }
    count_increases
}

fn sum_sliding_window(depths: &Vec<i32>) -> Vec<i32> {
    assert!(depths.len() >= 3);
    let mut sliding_window_sums = Vec::new();
    for i in 0..(depths.len() - 2) {
        sliding_window_sums.push(depths[i..i + 3].iter().sum());
    }
    sliding_window_sums
}

fn main() {
    let puzzle_input = read_puzzle_input("../input.txt");
    println!("{}", count_depth_increases(&puzzle_input));
    println!(
        "{}",
        count_depth_increases(&sum_sliding_window(&puzzle_input))
    );
}
