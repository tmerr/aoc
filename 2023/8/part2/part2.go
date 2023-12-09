package main

import (
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

type cursor struct {
	state   string
	instrIx int
}

type cacheResult struct {
	c          cursor
	deltaTotal int
}

type stateMachine struct {
	instructions string
	adj          map[string]Adj
	c            cursor
	total        int
	jumpToZCache map[cursor]cacheResult
	cacheHits    int
}

func (s *stateMachine) transition() {
	adj := s.adj[s.c.state]
	var nextC cursor
	switch s.instructions[s.c.instrIx] {
	case 'L':
		nextC.state = adj.L
	case 'R':
		nextC.state = adj.R
	default:
		panic("unrecognized instruction")
	}
	nextC.instrIx = (s.c.instrIx + 1) % len(s.instructions)
	s.c = nextC
	s.total++
}

func (s *stateMachine) transitionUntilEndZ() {
	if r, ok := s.jumpToZCache[s.c]; ok {
		s.c = r.c
		s.total += r.deltaTotal
		s.cacheHits++
		return
	}
	cSnapshot := s.c
	tSnapshot := s.total
	for {
		s.transition()
		if s.c.state[len(s.c.state)-1] == 'Z' {
			r := cacheResult{c: s.c, deltaTotal: s.total - tSnapshot}
			s.jumpToZCache[cSnapshot] = r
			fmt.Printf("added to cache: %+v -> %+v\n", cSnapshot, r)
			return
		}
	}
}

func pickStalestMachine(ms []*stateMachine) *stateMachine {
	winner := ms[0]
	for _, m := range ms {
		if m.total < winner.total {
			winner = m
		}
	}
	return winner
}

func machineTimesEqual(ms []*stateMachine) bool {
	t := ms[0].total
	for _, m := range ms {
		if m.total != t {
			return false
		}
	}
	return true
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
	var machines []*stateMachine
	jumpToZCache := make(map[cursor]cacheResult)
	for node, _ := range parsed.adj {
		if node[len(node)-1] == 'A' {
			machines = append(machines, &stateMachine{
				instructions: parsed.instructions,
				adj:          parsed.adj,
				c:            cursor{state: node},
				jumpToZCache: jumpToZCache,
			})
		}
	}
	for {
		stalest := pickStalestMachine(machines)
		stalest.transitionUntilEndZ()
		if machineTimesEqual(machines) {
			break
		}
	}
	fmt.Printf("Number of steps until all states end with Z: %d\n", machines[0].total)
	fmt.Printf("Cache hits for machine 0: %d\n", machines[0].cacheHits)
	return nil
}

func main() {
	if err := run(); err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}
}
