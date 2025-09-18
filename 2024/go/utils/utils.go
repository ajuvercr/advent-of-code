package utils

func Check(err any) {
  if err != nil {
    panic(err)

  }
}

func Split[T comparable](inp []T, value T) [][]T {
	var out [][]T
  var t []T
  for i := range(inp) {
    if value == inp[i] {
      out = append(out, t)
      t = make([]T, 0)
    } else {
      t = append(t, inp[i])
    }
  }
  return out
}

type Optional[T any] struct {
  Value T
  IsSet bool
}

func Empty[T any]() Optional[T] {
  return Optional[T]{ IsSet: false }
}

func Value[T any](value T) Optional[T] {
  out :=  Optional[T]{ IsSet: true}
  out.Value = value
  return out
}

type Grid[T any] struct {
  Content [][]T
}

func (r *Grid[T]) Get(x int, y int ) Optional[T] {
  if x >= len(r.Content)  || x<0 {
    return Empty[T]()
  }
  
  if y >= len(r.Content[x]) || y<0 {
    return Empty[T]()
  }

  return Value(r.Content[x][y])
}

