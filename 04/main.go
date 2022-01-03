package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type BingoBoard struct {
	numbers [5][5]int
}

type BingoGame struct {
	bingoBoard BingoBoard
	hitNumbers []int
}

func (b *BingoGame) callNumber(number int) {
	for _, row := range b.bingoBoard.numbers {
		for _, n := range row {
			if n == number {
				b.hitNumbers = append(b.hitNumbers, n)
				return
			}
		}
	}
}

func (b *BingoGame) hasWon() bool {
	if len(b.hitNumbers) < 5 {
		return false
	}

	// check rows
	for _, row := range b.bingoBoard.numbers {
		count := 0
		for _, n := range row {
			for _, calledNumber := range b.hitNumbers {
				if calledNumber == n {
					count += 1
				}
			}
		}
		if count == 5 {
			return true
		}
	}

	// check cols
	for colIdx := 0; colIdx < 5; colIdx++ {
		count := 0
		for _, row := range b.bingoBoard.numbers {
			for _, calledNumber := range b.hitNumbers {
				if calledNumber == row[colIdx] {
					count += 1
				}
			}
		}
		if count == 5 {
			return true
		}
	}

	return false
}

func (b *BingoGame) score() int {
	score := 0
	for _, row := range b.bingoBoard.numbers {
		for _, n := range row {
			flag := true
			for _, calledNumber := range b.hitNumbers {
				if n == calledNumber {
					flag = false
					break
				}
			}
			if flag {
				score += n
			}
		}
	}
	return score
}

func findFirstWinningGameScore(bingoBoards *[]BingoBoard, numbersToCall *[]int) (int, error) {
	bingoGames := []BingoGame{}
	for _, bingoBoard := range *bingoBoards {
		bingoGames = append(bingoGames, BingoGame{bingoBoard, []int{}})
	}

	for _, number := range *numbersToCall {
		for idx := range bingoGames {
			bingoGames[idx].callNumber(number)
			if bingoGames[idx].hasWon() {
				return bingoGames[idx].score() * number, nil
			}
		}
	}
	return 0, fmt.Errorf("No board won.")
}

func findLastWinningGameScore(bingoBoards *[]BingoBoard, numbersToCall *[]int) (int, error) {
	bingoGames := []BingoGame{}
	for _, bingoBoard := range *bingoBoards {
		bingoGames = append(bingoGames, BingoGame{bingoBoard, []int{}})
	}

	var winningBingoGame *BingoGame
	for _, number := range *numbersToCall {
		filteredBingoGames := []BingoGame{}
		for idx := range bingoGames {
			bingoGames[idx].callNumber(number)
			if !bingoGames[idx].hasWon() {
				filteredBingoGames = append(filteredBingoGames, bingoGames[idx])
			}
		}
		bingoGames = filteredBingoGames
		if len(bingoGames) == 1 {
			winningBingoGame = &bingoGames[0]
		}
		if len(bingoGames) == 0 {
			return winningBingoGame.score() * number, nil
		}
	}
	return 0, fmt.Errorf("No single board wins last.")
}

func parseNumbers(numberString string) (*[]int, error) {
	numbers := []int{}
	for _, numberString := range strings.Split(numberString, ",") {
		number, err := strconv.Atoi(numberString)
		if err != nil {
			return &[]int{}, fmt.Errorf("Could not convert to int: %w", err)
		}
		numbers = append(numbers, number)
	}
	return &numbers, nil
}

func parseBingoBoard(boardString string) (*BingoBoard, error) {
	bingoBoard := [5][5]int{}
	for rowIdx, row := range strings.Split(boardString, "\n") {
		for colIdx, numberString := range strings.Fields(row) {
			number, err := strconv.Atoi(numberString)
			if err != nil {
				return &BingoBoard{bingoBoard}, fmt.Errorf("Could not convert to int: %w", err)
			}
			bingoBoard[rowIdx][colIdx] = number
		}
	}
	return &BingoBoard{bingoBoard}, nil
}

func readPuzzleInput(filePath string) (*[]int, *[]BingoBoard, error) {
	fileContent, err := os.ReadFile(filePath)
	if err != nil {
		return &[]int{}, &[]BingoBoard{}, fmt.Errorf("File could not be read: %w", err)
	}
	fields := strings.Split(strings.TrimSpace(string(fileContent)), "\n\n")

	numbers, err := parseNumbers(fields[0])
	if err != nil {
		return &[]int{}, &[]BingoBoard{}, fmt.Errorf("Numbers could not be parsed: %w", err)
	}

	bingoBoards := []BingoBoard{}
	for _, field := range fields[1:] {
		bingoBoard, err := parseBingoBoard(field)
		if err != nil {
			return &[]int{}, &[]BingoBoard{}, fmt.Errorf("BingoBoards could not be parsed: %w", err)
		}
		bingoBoards = append(bingoBoards, *bingoBoard)
	}

	return numbers, &bingoBoards, nil
}

func main() {
	numbers, bingoBoards, err := readPuzzleInput("input.txt")
	if err != nil {
		panic(err)
	}

	score1, err := findFirstWinningGameScore(bingoBoards, numbers)
	if err != nil {
		panic(err)
	}
	fmt.Printf("The score of the first winning board is: %v\n", score1)

	score2, err := findLastWinningGameScore(bingoBoards, numbers)
	if err != nil {
		panic(err)
	}
	fmt.Printf("The score of the last winning board is: %v\n", score2)
}
