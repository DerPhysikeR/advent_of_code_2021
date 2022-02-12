use std::fs;

fn read_puzzle_input(filename: &str) -> Vec<i32> {
    let content = fs::read_to_string(filename).expect("Couldn't read file!");
    let mut depths = Vec::new();
    for line in content.lines() {
        depths.push(line.parse().unwrap());
    }
    depths
}

fn count_depth_increases(depths: &Vec<i32>) -> i32 {
    assert!(depths.len() >= 1);
    let mut count_increases = 0;
    let mut previous_depth = depths[0];
    for depth in depths {
        if depth > &previous_depth {
            count_increases += 1;
        }
        previous_depth = *depth;
    }
    count_increases
}

fn sum_sliding_window(depths: &Vec<i32>) -> Vec<i32> {
    let mut sliding_window_sums = Vec::new();
    for i in 0..(depths.len() - 2) {
        sliding_window_sums.push(depths[i] + depths[i + 1] + depths[i + 2]);
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
