package utils

import "strconv"

type Parser struct {
  data  string
  index int
}

func (parser *Parser) Int() (int, error) {
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


func (parser *Parser) End() bool {
  return parser.index == len(parser.data)
}

func (parser *Parser) Take() byte {
  out := parser.data[parser.index]
  parser.index ++
  return out
}

func (parser *Parser) Peek(def byte) byte {
  if parser.index == len(parser.data) {
    return def
  }
  return parser.data[parser.index]
}

func NewParser(data string) *Parser {
  p := Parser{data: data, index: 0}
  return &p
}
