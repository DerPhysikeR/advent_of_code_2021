use std::fs;

enum Direction {
    Up,
    Down,
    Forward,
}

struct Command {
    direction: Direction,
    units: u32,
}

#[derive(Clone, Copy, Debug)]
struct Vector {
    distance: u32,
    depth: u32,
}

fn parse_command(line: &str) -> Command {
    let parts: Vec<&str> = line.split(" ").collect();
    assert!(parts.len() == 2);
    let direction = match parts[0] {
        "up" => Direction::Up,
        "down" => Direction::Down,
        "forward" => Direction::Forward,
        _ => panic!("Can't parse {}", parts[0]),
    };
    let units: u32 = parts[1].parse().unwrap();
    Command { direction, units }
}

fn read_puzzle_input(filename: &str) -> Vec<Command> {
    let content = fs::read_to_string(filename).expect("Couldn't read input file");
    content.lines().map(|x| parse_command(x)).collect()
}

fn follow_course(course: &Vec<Command>) -> Vector {
    let mut location = Vector {
        distance: 0,
        depth: 0,
    };
    for command in course {
        match command.direction {
            Direction::Up => location.depth -= command.units,
            Direction::Down => location.depth += command.units,
            Direction::Forward => location.distance += command.units,
        }
    }
    location
}

fn follow_course_correctly(course: &Vec<Command>) -> Vector {
    let mut location = Vector {
        distance: 0,
        depth: 0,
    };
    let mut aim = 0;
    for command in course {
        match command.direction {
            Direction::Up => aim -= command.units,
            Direction::Down => aim += command.units,
            Direction::Forward => {
                location.distance += command.units;
                location.depth += aim * command.units;
            }
        }
    }
    location
}

fn main() {
    let puzzle_input = read_puzzle_input("../input.txt");
    let destination = follow_course(&puzzle_input);
    println!(
        "{:?} => {}",
        destination,
        destination.distance * destination.depth
    );
    let destination = follow_course_correctly(&puzzle_input);
    println!(
        "{:?} => {}",
        destination,
        destination.distance * destination.depth
    );
}
