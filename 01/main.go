package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func readPuzzleInput(filePath string) []int {
	file, ferr := os.Open(filePath)
	if ferr != nil {
		panic(ferr)
	}

	var heightMeasurements []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		heightMeasurement, converr := strconv.Atoi(line)
		if converr != nil {
			panic(converr)
		}
		heightMeasurements = append(heightMeasurements, heightMeasurement)
	}
	return heightMeasurements
}

func countDepthIncreases(depthMeasurements []int) int {
	previousDepth := depthMeasurements[0]
	depthIncreasesCounter := 0
	for _, depth := range depthMeasurements {
		if depth > previousDepth {
			depthIncreasesCounter++
		}
		previousDepth = depth
	}
	return depthIncreasesCounter
}

func main() {
	puzzleInput := readPuzzleInput("input.txt")
	fmt.Println(countDepthIncreases(puzzleInput))
}
