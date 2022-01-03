package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x int
	y int
}

func (p *Point) add(other Point) Point {
	return Point{p.x + other.x, p.y + other.y}
}

type Line struct {
	start Point
	end   Point
}

func findDelta(a int, b int) int {
	if a < b {
		return 1
	} else if a > b {
		return -1
	}
	return 0
}

func (l *Line) draw() *[]Point {
	points := []Point{l.start}
	delta := Point{findDelta(l.start.x, l.end.x), findDelta(l.start.y, l.end.y)}
	point := l.start
	for point != l.end {
		point = point.add(delta)
		points = append(points, point)
	}
	return &points
}

func (l *Line) isOrthogonal() bool {
	return l.start.x == l.end.x || l.start.y == l.end.y
}

func countOverlappingPoints(lines *[]Line) int {
	pointCounts := map[Point]int{}
	for _, line := range *lines {
		for _, point := range *line.draw() {
			pointCounts[point] += 1
		}
	}
	count := 0
	for _, pc := range pointCounts {
		if pc > 1 {
			count += 1
		}
	}
	return count
}

func getOrthogonalLines(lines *[]Line) *[]Line {
	orthogonalLines := []Line{}
	for _, line := range *lines {
		if line.isOrthogonal() {
			orthogonalLines = append(orthogonalLines, line)
		}
	}
	return &orthogonalLines
}

func parsePoint(pointString string) Point {
	coords := []int{}
	for _, coordString := range strings.Split(pointString, ",") {
		coord, err := strconv.Atoi(coordString)
		if err != nil {
			panic(err)
		}
		coords = append(coords, coord)
	}
	return Point{coords[0], coords[1]}
}

func readPuzzleInput(filePath string) *[]Line {
	lines := []Line{}
	data, err := os.ReadFile(filePath)
	if err != nil {
		panic(err)
	}
	for _, lineString := range strings.Split(strings.TrimSpace(string(data)), "\n") {
		parts := strings.Split(lineString, " -> ")
		start := parsePoint(parts[0])
		end := parsePoint(parts[1])
		lines = append(lines, Line{start, end})
	}
	return &lines
}

func main() {
	lines := readPuzzleInput("input.txt")
	fmt.Println(countOverlappingPoints(getOrthogonalLines(lines)))
	fmt.Println(countOverlappingPoints(lines))
}
