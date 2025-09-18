package days

import (
  "os"
  "fmt"
  "aoc/utils"
)

 
func count(grid utils.Grid[byte], x int, y int, i int, todo []byte, options [][]int) int {
  if len(todo) == i {
    return 1
  }
  v := grid.Get(x, y)
  if !v.IsSet || v.Value != todo[i] {
    return 0
  }

  if len(todo) == i + 1 {
    return 1
  }

  out := 0


  for j:= range(options) {
    delta := options[j]
    p := count(grid, x+delta[0], y+delta[1], i+1, todo, options)
    out += p
  }

  return out
}

func part2Valid(grid utils.Grid[byte], x int, y int) int {
  v := grid.Get(x, y)
  if !v.IsSet || v.Value != 'A' {
    return 0
  }

  options := [][]int{
    {1, 1},
    {1, -1},
    {-1, -1},
    {-1, 1},
  }

  var things []byte
  for k := range(options) {
    o := options[k]
    p := grid.Get(x + o[0], y+o[1])
    if p.IsSet {
      things =append(things, p.Value)
    }
  }

  ms := 0
  ss := 0
  for i := range(things) {
    if things[i] == 'M' {
      ms += 1
    }

    if things[i] == 'S' {
      ss += 1
    }
  }

  if ms != 2 || ss != 2 {
    return 0
  }

  if things[0] == things[1] || things[1] == things[2] || things[2] == things[3] {
    return 1
  }
  return 0
}

func Day04() {
	dat, err := os.ReadFile("./input/04.txt")
	// dat, err := os.ReadFile("./example.txt")
	if err != nil {
		panic(err)
	}

  lines := utils.Split(dat, '\n')
  grid := utils.Grid [byte]{ Content: lines}

  part1 := 0
  part2 := 0
  for i := 0; i< len(grid.Content); i++ {
    for j := 0; j< len(grid.Content[i]); j++ {
      options := [][]int{
        {0, 1},
        {1, 1},
        {1, 0},
        {1, -1},
        {0, -1},
        {-1, -1},
        {-1, 0},
        {-1, 1},
      }
      for k := range(options) {
          p := count(grid, i, j, 0, []byte("XMAS"), [][]int{options[k]})
          part1 += p
      }
      part2 += part2Valid(grid, i, j)
    }
  }

	fmt.Println("part1:", part1)
	fmt.Println("part2:", part2)
}

