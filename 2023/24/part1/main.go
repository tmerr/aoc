package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Obj struct {
	S [3]float64
	V [3]float64
}

func parseVec(csv string) ([3]float64, error) {
	var res [3]float64
	for i, nstr := range strings.Split(csv, ",") {
		n, err := strconv.ParseInt(nstr, 10, 64)
		if err != nil {
			return [3]float64{}, err
		}
		res[i] = float64(n)
	}
	return res, nil
}

func intersects(a, b Obj) bool {
	denom := (a.V[0]*b.V[1] - a.V[1]*b.V[0])
	if denom == 0 {
		return false
	}
	t1 := (-a.S[0]*b.V[1] + a.S[1]*b.V[0] + b.S[0]*b.V[1] - b.S[1]*b.V[0]) / denom
	t2 := (-a.S[0]*a.V[1] + a.S[1]*a.V[0] - a.V[0]*b.S[1] + a.V[1]*b.S[0]) / denom
	if t1 < 0 {
		return false
	}
	if t2 < 0 {
		return false
	}
	x := a.S[0] + a.V[0]*t1
	y := a.S[1] + a.V[1]*t1
	if x < 200000000000000.0 || x > 400000000000000.0 {
		return false
	}
	if y < 200000000000000.0 || y > 400000000000000.0 {
		return false
	}
	return true
}

func run() error {
	bs, err := os.ReadFile("input.txt")
	if err != nil {
		return err
	}
	txt := strings.ReplaceAll(string(bs), " ", "")
	txt = strings.ReplaceAll(txt, "\r", "")
	var objs []Obj
	for _, line := range strings.Split(txt, "\n") {
		if len(line) == 0 {
			continue
		}
		sides := strings.Split(line, "@")
		if len(sides) != 2 {
			return fmt.Errorf("expected 2 sides, got %d", len(sides))
		}
		var obj Obj
		obj.S, err = parseVec(sides[0])
		if err != nil {
			return err
		}
		obj.V, err = parseVec(sides[1])
		if err != nil {
			return err
		}
		objs = append(objs, obj)
	}
	s := 0
	for j := 0; j < len(objs); j++ {
		for i := j + 1; i < len(objs); i++ {
			if intersects(objs[j], objs[i]) {
				s += 1
			}
		}
	}
	fmt.Println(s)
	return nil
}

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
