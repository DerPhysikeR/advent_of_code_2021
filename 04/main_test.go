package main

import (
	"testing"
)

func TestFindFirstWinningGameScore(t *testing.T) {

	t.Run("Calc result of part 1 with test input", func(t *testing.T) {
		numbers, bingoBoards, err := readPuzzleInput("test_input.txt")
		if err != nil {
			t.Fatal("Test input could not be read.")
		}
		score, err := findFirstWinningGameScore(bingoBoards, numbers)
		if err != nil {
			t.Fatal("No winning board found")
		}
		want := 4512
		if score != want {
			t.Errorf("got %v want %v", score, want)
		}
	})

}

func TestBingoGame(t *testing.T) {
    var bingoBoard = BingoBoard{[5][5]int{
			{1, 2, 3, 4, 5},
			{6, 7, 8, 9, 10},
			{11, 12, 13, 14, 15},
			{16, 17, 18, 19, 20},
			{21, 22, 23, 24, 25},
		}}

	t.Run("Test callNumber", func(t *testing.T) {
		bingoGame := BingoGame{bingoBoard, []int{}}
		bingoGame.callNumber(0)
		bingoGame.callNumber(1)
		if len(bingoGame.hitNumbers) != 1 || bingoGame.hitNumbers[0] != 1 {
			t.Errorf("Expected %v, got %v", []int{1}, bingoGame.hitNumbers)
		}
	})

	t.Run("Test if board wins by row", func(t *testing.T) {
		numbers := []int{1, 2, 3, 4, 5}
		bingoGame := BingoGame{bingoBoard, numbers}
		if !bingoGame.hasWon() {
			t.Errorf("Winning BingoGame didn't win")
		}
	})

	t.Run("Test if board wins by column", func(t *testing.T) {
		numbers := []int{1, 6, 11, 16, 21}
		bingoGame := BingoGame{bingoBoard, numbers}
		if !bingoGame.hasWon() {
			t.Errorf("Winning BingoGame didn't win")
		}
	})

	t.Run("Test board score", func(t *testing.T) {
		numbers := []int{1, 2, 3, 4, 5}
		bingoGame := BingoGame{bingoBoard, numbers}
		got := bingoGame.score()
		want := 25*13 - 15
		if want != got {
			t.Errorf("Expected %v, got %v", want, got)
		}
	})

}
