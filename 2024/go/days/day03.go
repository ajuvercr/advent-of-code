package days

import (
	"os"
  "regexp"
  "fmt"
	"strconv"
)

func Day03() {
	dat, err := os.ReadFile("./input/03.txt")
	// dat, err := os.ReadFile("./example.txt")
	if err != nil {
		panic(err)
	}

    r := regexp.MustCompile("(mul\\((?P<x>[0-9]{1,3}),(?P<y>[0-9]{1,3})\\)|do\\(()\\)|don't\\(\\))");


  part1 := 0
  part2 := 0
  enabled := true

  arr := r.FindAllStringSubmatch(string(dat), 100000)

  for i:= 0; i < len(arr); i++ {
    a := arr[i]
    if a[0] == "do()" {
      enabled = true
      continue;
    }

    if a[0] == "don't()" {
      enabled = false
      continue;
    }
    x, _ := strconv.Atoi(a[2])
    y, _ := strconv.Atoi(a[3])
    t := x*y
    part1 += t
    if enabled {
      part2 += t
    }
  }

  fmt.Println("Part1:", part1)
  fmt.Println("Part2:", part2)
}

