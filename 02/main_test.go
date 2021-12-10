package main

import (
	"testing"
)

func TestFollowCourse(t *testing.T) {
	t.Run("test follow course with test input", func(t *testing.T) {
		puzzleInput := readPuzzleInput("test_input.txt")
		got := followCourse(puzzleInput)
		want := Location{15, 10}
		if got != want {
			t.Errorf("got %q want %q", got, want)
		}
	})

	t.Run("test follow course correctly with test input", func(t *testing.T) {
		puzzleInput := readPuzzleInput("test_input.txt")
		got := followCourseCorrectly(puzzleInput)
		want := Location{15, 60}
		if got != want {
			t.Errorf("got %q want %q", got, want)
		}
	})
}
