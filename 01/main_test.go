package main

import "testing"

func TestCountDepthIncreases(t *testing.T) {

	t.Run("test depth count with test input", func(t *testing.T) {
		got := countDepthIncreases(readPuzzleInput("test_input.txt"))
		want := 7
		if got != want {
			t.Errorf("got %q want %q", got, want)
		}
	})

	t.Run("test windowed depth count with test input", func(t *testing.T) {
		got := countWindowedDepthIncreases(readPuzzleInput("test_input.txt"))
		want := 5
		if got != want {
			t.Errorf("got %q want %q", got, want)
		}
	})

}
