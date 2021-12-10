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

func sumSlidingWindow(values []int) []int {
	var windowedValues []int
	for i := range values {
		if i > len(values)-3 {
			break
		}
		windowedValues = append(windowedValues, values[i]+values[i+1]+values[i+2])
	}
	return windowedValues
}

func countWindowedDepthIncreases(depthMeasurements []int) int {
	slidingWindowSums := sumSlidingWindow(depthMeasurements)
	return countDepthIncreases(slidingWindowSums)

}

func main() {
	puzzleInput := readPuzzleInput("input.txt")
	fmt.Println(countDepthIncreases(puzzleInput))
	fmt.Println(countWindowedDepthIncreases(puzzleInput))
}
