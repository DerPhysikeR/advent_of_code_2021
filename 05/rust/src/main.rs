use std::collections::HashMap;
use std::fs;

#[derive(Debug, Copy, Clone, Hash, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Eq for Point {}

impl Point {
    fn from_str(string: &str) -> Self {
        let mut parts = string.split(",").map(|x| x.parse().unwrap()).take(2);
        Self {
            x: parts.next().unwrap(),
            y: parts.next().unwrap(),
        }
    }
}

#[derive(Debug)]
struct Line {
    start: Point,
    end: Point,
}

impl Line {
    fn from_str(string: &str) -> Self {
        let mut parts = string.split(" -> ").map(|p| Point::from_str(&p)).take(2);
        Line {
            start: parts.next().unwrap(),
            end: parts.next().unwrap(),
        }
    }

    fn is_orthogonal(&self) -> bool {
        self.start.x == self.end.x || self.start.y == self.end.y
    }
}

impl<'a> IntoIterator for &'a Line {
    type Item = Point;
    type IntoIter = LineIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        LineIterator {
            line: &self,
            index: 0,
        }
    }
}

struct LineIterator<'a> {
    line: &'a Line,
    index: i32,
}

impl Iterator for LineIterator<'_> {
    type Item = Point;

    fn next(&mut self) -> Option<Point> {
        let dx = get_delta(&self.line.start.x, &self.line.end.x);
        let dy = get_delta(&self.line.start.y, &self.line.end.y);
        let point = Point {
            x: self.line.start.x + self.index * dx,
            y: self.line.start.y + self.index * dy,
        };
        self.index += 1;
        if self.line.end.x + dx == point.x && self.line.end.y + dy == point.y && self.index > 1 {
            return None;
        }
        Some(point)
    }
}

fn get_delta(start: &i32, end: &i32) -> i32 {
    if start < end {
        return 1;
    }
    if start > end {
        return -1;
    }
    0
}

#[cfg(test)]
mod tests {
    use crate::{count_overlapping_points, read_puzzle_input, Line, Point};

    #[test]
    fn line_iteration_in_x_and_y_directions() {
        let line = Line {
            start: Point { x: 0, y: 0 },
            end: Point { x: -3, y: 3 },
        };
        assert!(4 == line.into_iter().count());
        for (i, point) in line.into_iter().enumerate() {
            assert!(i32::try_from(i).unwrap() == -point.x);
            assert!(i32::try_from(i).unwrap() == point.y);
        }
    }

    #[test]
    fn line_iteration_in_x_and_y_directions_starting_somewhere() {
        let line = Line {
            start: Point { x: 1, y: -1 },
            end: Point { x: -3, y: 3 },
        };
        assert!(5 == line.into_iter().count());
        for (i, point) in line.into_iter().enumerate() {
            assert!(i32::try_from(i).unwrap() - 1 == -point.x);
            assert!(i32::try_from(i).unwrap() - 1 == point.y);
        }
    }

    #[test]
    fn line_iteration_for_start_eq_end_point() {
        let line = Line {
            start: Point { x: 0, y: 0 },
            end: Point { x: 0, y: 0 },
        };
        assert!(1 == line.into_iter().count());
        for (i, point) in line.into_iter().enumerate() {
            assert!(i32::try_from(i).unwrap() == point.x);
            assert!(i32::try_from(i).unwrap() == point.y);
        }
    }

    #[test]
    fn count_overlapping_points_for_orthogonal_lines_in_test_input() {
        let puzzle_input = read_puzzle_input("../test_input.txt");
        assert!(5 == count_overlapping_points(puzzle_input.iter().filter(|l| l.is_orthogonal())));
    }

    #[test]
    fn count_overlapping_points_for_test_input() {
        let puzzle_input = read_puzzle_input("../test_input.txt");
        assert!(12 == count_overlapping_points(puzzle_input.iter()));
    }
}

fn count_overlapping_points<'a, I>(lines: I) -> usize
where
    I: Iterator<Item = &'a Line>,
{
    let mut point_counter = HashMap::new();
    for line in lines {
        for point in line.into_iter() {
            *point_counter.entry(point).or_insert(0) += 1;
        }
    }
    point_counter.iter().filter(|&(_, c)| *c >= 2).count()
}

fn read_puzzle_input(filepath: &str) -> Vec<Line> {
    let content = fs::read_to_string(filepath).expect("Couldn't read input file");
    content
        .trim()
        .split("\n")
        .map(|l| Line::from_str(&l))
        .collect()
}

fn main() {
    let puzzle_input = read_puzzle_input("../input.txt");
    println!(
        "{}",
        count_overlapping_points(puzzle_input.iter().filter(|l| l.is_orthogonal()))
    );
    println!("{}", count_overlapping_points(puzzle_input.iter()));
}
