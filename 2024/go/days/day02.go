package days

import (
	"fmt"
	"os"
  "aoc/utils"
)


func isValid(report []int, skip int) bool {
  increasing := false
  decreasing := false

  last := report[0]

  for i := 1; i<len(report); i++ {
    if i == skip {
      continue;
    }

    current := report[i]

    diff := last - current
    if diff > 3 || diff < -3 {
      return false
    }

    if diff > 0 {
      increasing = true
    }

    if diff < 0 {
      decreasing = true 
    }

    if current == last {
      return false
    }

    last = current
  }

  if increasing && decreasing {
    return false
  }

  return true
}

func Day02() {
	dat, err := os.ReadFile("./input/02.txt")
	// dat, err := os.ReadFile("./example.txt")
	if err != nil {
		panic(err)
	}

  parser := utils.NewParser(string(dat))
	var reports [][]int

  for !parser.End() {
    var report []int

    n, err := parser.Int()
    report = append(report, n)

    for parser.Take() != '\n' {
      n, err = parser.Int()
      if err != nil {
        break
      }
      report = append(report, n)
    }

      if err != nil {
        break
      }

    reports = append(reports, report)
  }

  part1 := 0
  part2 := 0
  for i := 0; i < len(reports); i++ {
    if isValid(reports[i], -1) {
      part1 ++
    }

    // check if we should skip the first one
    if isValid(reports[i][1:], -1) {
      part2 ++;
      continue;
    }

    for j := 0; j < len(reports[i]); j++ {
      if isValid(reports[i], j) {
        part2 ++;
        break;
      }
    }
  }

	fmt.Println("part1:", part1)
	fmt.Println("part2:", part2)
}

