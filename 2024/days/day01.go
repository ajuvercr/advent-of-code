package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
)

func main() {
	dat, err := os.ReadFile("./input/01.txt")
	// dat, err := os.ReadFile("./example.txt")
	if err != nil {
		panic(err)
	}

	var left []int
	var right []int

	currentLeft := true
	c := 0

	for i := 0; i < len(dat); i++ {
		isNumber := dat[i] <= '9' && dat[i] >= '0'
		if isNumber {
			continue
		}
		if c != i {
			n, err := strconv.Atoi(string(dat[c:i]))
			if err != nil {
				panic(err)
			}
			if currentLeft {
				left = append(left, n)
			} else {
				right = append(right, n)
			}
			currentLeft = !currentLeft
		}
		c = i + 1
	}

	slices.Sort(left)
	slices.Sort(right)

	part1 := 0

	for i := 0; i < len(left); i++ {
		diff := left[i] - right[i]
		if diff < 0 {
			part1 -= diff
		} else {
			part1 += diff
		}
	}

	fmt.Println("part1:", part1)

	part2 := 0
	j := 0

	for i := 0; i < len(left); i++ {
		for right[j] < left[i] {
			j++
		}
		count := 0
		for right[j+count] == left[i] {
			count++
		}

		part2 += count * left[i]
	}

	fmt.Println("part2:", part2)
}

