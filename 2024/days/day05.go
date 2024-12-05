package days

import (
	"fmt"
	"os"
  "aoc/utils"
)

type rule struct {
  input int
  output int
}


func (rule *rule) valid(inputs []int, index int) bool {
  x := inputs[index]

  if x == rule.input {
    for j := 0; j < index; j++ {
      if inputs[j] == rule.output {
        return false
      }
    }
  }

  if x == rule.output {
    for j := index + 1; j < len(inputs); j++ {
      if inputs[j] == rule.input {
        return false
      }
    }
  }
  return true;
}

func remove[T any](slice []T, s int) []T {
    return append(slice[:s], slice[s+1:]...)
}

func order(items []int, rules []rule) []int {
  var out []int
  lastI := len(items)

  for len(items) > 0 {
    for i := range(items) {
      valid := true
      item := items[i]
      for r := range(rules) {

        if rules[r].output == item {
          for k := range(items) {
            if rules[r].input == items[k] {
              valid = false
            }
          }
          if !valid {
            break
          }
        }

      }

      if valid {
        items = remove(items, i)
        out = append(out, item)
        break
      }
    }

    if len(items) == lastI {
	    fmt.Println("Aaaah", items, rules)
      panic("Didn't find a correct item :(")
    }
    lastI = len(items)
  }



  return out
}


func Day05() {
	dat, err := os.ReadFile("./input/05.txt")
	// dat, err := os.ReadFile("./example.txt")
	if err != nil {
		panic(err)
	}

  var rules []rule
  parser := utils.NewParser(string(dat))

  for parser.Peek(' ') != '\n' {
    input, err := parser.Int()
    utils.Check(err)
    parser.Take()

    output, err := parser.Int()
    utils.Check(err)
    parser.Take()

    rules = append(rules, rule { 
      input: input,
      output: output,
    })
  } 

  part1 := 0
  part2 := 0
  for !parser.End() {
    var items []int
    item, err := parser.Int()
    utils.Check(err)
    items = append(items, item)

    for parser.Take() == ',' {
      item, err := parser.Int()
      utils.Check(err)
      items = append(items, item)
    }

    valid := true
    for j := range(items) {
      for r := range(rules) {
        if !rules[r].valid(items, j) {
          valid = false
          ordered := order(items, rules)
          part2 += ordered[len(ordered)/2]
          break
        }
      }
      if !valid {
        break
      }
    }

    if valid {
      part1 += items[len(items)/2]

    }

  }



	fmt.Println("part1:", part1)


	fmt.Println("part2:", part2)
}

