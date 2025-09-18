package days

import (
	"aoc/utils"
	"fmt"
	"os"
)
type Direction int


const (
    Up Direction = iota
    Right
    Down
    Left
)

func add(x int, dir Direction) int {
  return x | (2 << dir)
}

func getDef(coord Coord, seen map[Coord]int, def int) int {
  v, ok := seen[coord]
  if ok {
    return v
  }
  return def
}

func hasNext(bits int, current Direction) bool {
  n := next(current)
  return bits & (2 << n) != 0
}

func next(dir Direction) Direction {
  return (dir + 1) % 4
}

func step(start Coord, dir Direction, obstacles_w_c [][]int, obstacles_c_w [][]int, seen map[Coord]int, total *int) utils.Optional[Coord] {
  switch dir {
    case Up:
      ys := obstacles_w_c[start.x]
      for i := len(ys) - 1; i >= 0; i -- {
        if ys[i] < start.y {

          for k := ys[i]+1; k <= start.y; k++ {
            target := Coord { y: k, x: start.x}
            v := getDef(target, seen, 0)
            if hasNext(v, dir) {
              *total ++ 
            }
            seen[target] = add(v, dir)
          }

          return utils.Optional[Coord] {
            IsSet: true,
            Value: Coord {
              x: start.x,
              y: ys[i] +1,
            },
          }
        }
      }

    case Down:
      ys := obstacles_w_c[start.x]
      for i := 0; i < len(ys); i ++ {
        if ys[i] > start.y {
          for k := start.y; k < ys[i]; k++ {
            target := Coord { y: k, x: start.x}
            v := getDef(target, seen, 0)
            if hasNext(v, dir) {
              *total ++ 
            }
            seen[target] = add(v, dir)
          }

          return utils.Optional[Coord] {
            IsSet: true,
            Value: Coord {
              x: start.x,
              y: ys[i] -1,
            },
          }
        }
      }

    case Right:
      ys := obstacles_c_w[start.y]
      for i := 0; i < len(ys); i ++ {
        if ys[i] > start.x {
        
          for k := start.x; k < ys[i]; k++ {
            target := Coord { x: k, y: start.y}
            v := getDef(target, seen, 0)
            if hasNext(v, dir) {
              *total ++ 
            }
            seen[target] = add(v, dir)
          }

          return utils.Optional[Coord] {
            IsSet: true,
            Value: Coord {
              y: start.y,
              x: ys[i] -1,
            },
          }
        }
      }

    case Left:
      ys := obstacles_c_w[start.y]
      for i := len(ys) - 1; i >= 0; i -- {
        if ys[i] < start.x {

          for k := ys[i]+1; k <= start.x; k++ {
            target := Coord { x: k, y: start.y}
            v := getDef(target, seen, 0)
            if hasNext(v, dir) {
              *total ++ 
            }
            seen[target] = add(v, dir)
          }

          return utils.Optional[Coord] {
            IsSet: true,
            Value: Coord {
              y: start.y,
              x: ys[i] +1,
            },
          }
        }
      }
  }

  return utils.Empty[Coord]()
}

type Coord struct {
    x int
    y int
}

func Day06() {
	dat, err := os.ReadFile("./input/06.txt")
	// dat, err := os.ReadFile("./example.txt")
	if err != nil {
		panic(err)
	}

  var obstacles_w_c [][]int
  var obstacles_c_w [][]int

  seen := make(map[Coord]int)
  var locations []Coord

  lines := utils.Split(dat, '\n')
  var start Coord

  for i:=0; i < len(lines); i++ {
    obstacles_w_c = append(obstacles_w_c, []int{})

    for j:=0; j<len(lines[i]); j++ {
      if len(obstacles_c_w) <= j {
        obstacles_c_w = append(obstacles_c_w, []int{}) }

      if lines[i][j] == '#' {
        obstacles_w_c[i] = append(obstacles_w_c[i], j)
        obstacles_c_w[j] = append(obstacles_c_w[j], i)
      }

      if lines[i][j] == '^' {
        start = Coord {
          y: i,
          x: j,
        }
      }
    }
  }

	fmt.Println("parsed:", start, obstacles_c_w, obstacles_w_c)
  current := utils.Value(start)
  direction := Up
  total := 0
  for current.IsSet {
    fmt.Println("next", current)
    locations = append(locations, current.Value)
    current = step(current.Value, direction, obstacles_c_w, obstacles_w_c, seen, &total)
    direction = next(direction)
  }

  for k := range seen {
      lines[k.y][k.x] = 'X'
  }
  for i := range lines {
    fmt.Println(string(lines[i]))
  }

	fmt.Println("part1:", len(seen) + 3)
	fmt.Println("part2:", locations)
  // 224 too low
  // I only check if the person directly is in a loop
  // Not if they will eventually get in the loop
	fmt.Println("part2:", total)
}

