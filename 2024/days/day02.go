package days

import (
	"fmt"
	"os"
	"strconv"
)

type parser struct {
  data  string
  index int
}

func ParserInt(parser *parser) (int, error) {
  var start int
  started := false

	for parser.index < len(parser.data) {
		isNumber := parser.data[parser.index] <= '9' && parser.data[parser.index] >= '0'
    if isNumber {
      if !started {
        start = parser.index
        started = true
      }
    } else {
      if started {
        break
      }
    }

      parser.index ++
  }

	return strconv.Atoi(string(parser.data[start:parser.index]))
}

func ParserEnd(parser *parser) bool {
  return parser.index == len(parser.data)
}

func ParserTake(parser *parser) byte {
  out := parser.data[parser.index]
  parser.index ++
  return out
}

func newParser(data string) *parser {
  p := parser{data: data, index: 0}
  return &p
}

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
	// dat, err := os.ReadFile("./example.txt")
	dat, err := os.ReadFile("./input/02.txt")
	// dat, err := os.ReadFile("./example.txt")
	if err != nil {
		panic(err)
	}

  parser := newParser(string(dat))
	var reports [][]int

  for !ParserEnd(parser) {
    var report []int

    n, err := ParserInt(parser)
    report = append(report, n)

    for ParserTake(parser) != '\n' {
      n, err = ParserInt(parser)
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

