package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Direction int

const (
	forward Direction = iota
	up
	down
	unspecified
)

type Command struct {
	direction Direction
	units     int
}

type Location struct {
	distance int
	depth    int
}

func parseDirection(directionString string) (Direction, error) {
	switch directionString {
	case "up":
		return up, nil
	case "forward":
		return forward, nil
	case "down":
		return down, nil
	}
	return unspecified, fmt.Errorf("The string '%s' was not recognized as a direction.", directionString)
}

func parseCommand(line string) Command {
	s := strings.Fields(line)
	command, err := parseDirection(s[0])
	if err != nil {
		panic(err)
	}
	units, err := strconv.Atoi(s[1])
	if err != nil {
		panic(err)
	}
	return Command{command, units}
}

func readPuzzleInput(filePath string) []Command {
	file, err := os.Open(filePath)
	if err != nil {
		panic(err)
	}
	var commands []Command
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		commands = append(commands, parseCommand(line))
	}
	return commands
}

func followCourse(course []Command) Location {
	location := Location{0, 0}
	for _, command := range course {
		switch command.direction {
		case up:
			location = Location{location.distance, location.depth - command.units}
		case down:
			location = Location{location.distance, location.depth + command.units}
		case forward:
			location = Location{location.distance + command.units, location.depth}
		}
	}
	return location
}

func main() {
	puzzleInput := readPuzzleInput("input.txt")
	targetLocation := followCourse(puzzleInput)
	fmt.Printf("%d\n", targetLocation.depth*targetLocation.distance)
}
