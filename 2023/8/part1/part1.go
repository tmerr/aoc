package main

import (
	"errors"
	"fmt"
	"os"
)

func tokenize(bs []byte) []string {
	var res []string
	const SKIPPING = -1
	i := SKIPPING
	j := 0
	endOfToken := func() {
		if i != SKIPPING {
			res = append(res, string(bs[i:j]))
			i = SKIPPING
		}
	}
	for j < len(bs) {
		switch bs[j] {
		case '(', ')', '=', ',':
			endOfToken()
			i = j
			j++
			endOfToken()
		case '\r', '\n', ' ', '\t':
			endOfToken()
			j++
		default:
			if i == SKIPPING {
				i = j
			}
			j++
		}
	}
	return res
}

type declaredToken func(string) error

func tokExpect(tok string) declaredToken {
	return func(s string) error {
		if tok != s {
			return fmt.Errorf("got token %q, expected %q", s, tok)
		}
		return nil
	}
}

func tokCapture(dest *string) declaredToken {
	return func(s string) error {
		*dest = s
		return nil
	}
}

func consumeTokens(ts []declaredToken, toks []string) ([]string, error) {
	for len(ts) > 0 && len(toks) > 0 {
		var t declaredToken
		t, ts = ts[0], ts[1:]
		var tok string
		tok, toks = toks[0], toks[1:]
		if err := t(tok); err != nil {
			return nil, err
		}
	}
	return toks, nil
}

type Adj struct {
	L string
	R string
}

type parsed struct {
	instructions string
	adj          map[string]Adj
}

func parse(bs []byte) (parsed, error) {
	toks := tokenize(bs)
	res := parsed{adj: make(map[string]Adj)}
	var err error
	toks, err = consumeTokens(
		[]declaredToken{tokCapture(&res.instructions)},
		toks)
	if err != nil {
		return parsed{}, err
	}
	for len(toks) > 0 {
		var node string
		var adj Adj
		toks, err = consumeTokens(
			[]declaredToken{
				tokCapture(&node),
				tokExpect("="),
				tokExpect("("),
				tokCapture(&adj.L),
				tokExpect(","),
				tokCapture(&adj.R),
				tokExpect(")"),
			},
			toks)
		if err != nil {
			return parsed{}, err
		}
		res.adj[node] = adj
	}
	return res, nil
}

func run() error {
	bs, err := os.ReadFile("input.txt")
	if err != nil {
		return err
	}
	parsed, err := parse(bs)
	if err != nil {
		return err
	}
	state := "AAA"
	i := 0
	// Note: this won't necessarily terminate on all inputs.
	for ; state != "ZZZ"; i++ {
		adj := parsed.adj[state]
		wrapped := i % len(parsed.instructions)
		switch parsed.instructions[wrapped] {
		case 'L':
			state = adj.L
		case 'R':
			state = adj.R
		default:
			return errors.New("unrecognized instruction")
		}
	}
	fmt.Printf("Number of steps to reach ZZZ: %d\n", i)
	return nil
}

func main() {
	if err := run(); err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}
}
