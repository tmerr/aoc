package main

import (
	"fmt"
	"os"
	"strings"
)

type Pulse struct {
	sender    Module
	val       bool
	receivers []Module
}

type Module interface {
	Init(inputs, outputs []Module)
	React(Pulse) (Pulse, bool)
}

type FlipFlop struct {
	outputs []Module
	state   bool
	inp     bool
}

func (f *FlipFlop) Init(_, outputs []Module) {
	f.outputs = outputs
}

func (f *FlipFlop) React(p Pulse) (Pulse, bool) {
	if p.val {
		// on high pulse, nothing happens.
		return Pulse{}, false
	}
	f.state = !f.state
	return Pulse{sender: f, val: f.state, receivers: f.outputs}, true
}

type Conjunction struct {
	outputs []Module
	memory  map[Module]bool
}

func (c *Conjunction) Init(inputs, outputs []Module) {
	c.memory = make(map[Module]bool)
	for _, m := range inputs {
		// initially default to remembering a low pulse
		c.memory[m] = false
	}
	c.outputs = outputs
}

func (c *Conjunction) React(p Pulse) (Pulse, bool) {
	c.memory[p.sender] = p.val
	res := Pulse{sender: c, val: false, receivers: c.outputs}
	for _, v := range c.memory {
		if !v {
			res.val = true
		}
	}
	return res, true
}

type Broadcaster struct {
	outputs []Module
}

func (b *Broadcaster) Init(_, outputs []Module) {
	b.outputs = outputs
}

func (b *Broadcaster) React(p Pulse) (Pulse, bool) {
	return Pulse{sender: b, val: p.val, receivers: b.outputs}, true
}

type NoopModule struct{}

func (n *NoopModule) Init(_, outputs []Module) {}

func (n *NoopModule) React(_ Pulse) (Pulse, bool) {
	return Pulse{}, false
}

func propagate(b *Broadcaster) (lowPulses, highPulses int) {
	buttonPulse := Pulse{val: false, receivers: []Module{b}}
	var q []Pulse
	lowPulses++
	p, ok := b.React(buttonPulse)
	if ok {
		q = append(q, p)
	}
	for len(q) > 0 {
		p, q = q[0], q[1:]
		if p.val {
			highPulses += len(p.receivers)
		} else {
			lowPulses += len(p.receivers)
		}
		for _, r := range p.receivers {
			p1, ok := r.React(p)
			if ok {
				q = append(q, p1)
			}
		}
	}
	return
}

func run() error {
	bytez, err := os.ReadFile("input.txt")
	if err != nil {
		return err
	}
	broadcaster := &Broadcaster{}
	modules := make(map[string]Module)
	moduleToOuts := make(map[string][]string)
	moduleToIns := make(map[string][]string)
	// windows pls.
	contents := strings.ReplaceAll(string(bytez), "\r", "")
	for _, line := range strings.Split(contents, "\n") {
		line = strings.Trim(line, " \n")
		if line == "" {
			continue
		}
		var module string
		var outs []string
		for _, tok := range strings.Split(line, " ") {
			if module == "" {
				module = tok
				continue
			}
			if tok == "->" {
				continue
			}
			tok = strings.TrimSuffix(tok, ",")
			outs = append(outs, tok)
		}
		switch module[0] {
		case '%':
			module = module[1:]
			modules[module] = &FlipFlop{}
		case '&':
			module = module[1:]
			modules[module] = &Conjunction{}
		default:
			if module != "broadcaster" {
				panic("unknown module type " + module)
			}
			modules[module] = broadcaster
		}
		moduleToOuts[module] = outs
		for _, o := range outs {
			moduleToIns[o] = append(moduleToIns[o], module)
		}
	}
	for m := range modules {
		var outs []Module
		for _, o := range moduleToOuts[m] {
			m2, ok := modules[o]
			if !ok {
				// Not all modules are explicitly declared.
				// These are called "untyped modules" in the problem.
				m2 = &NoopModule{}
				modules[o] = m2
			}
			outs = append(outs, m2)
		}
		var ins []Module
		for _, i := range moduleToIns[m] {
			m2, ok := modules[i]
			if !ok {
				return fmt.Errorf("no module %q", i)
			}
			ins = append(ins, m2)
		}
		modules[m].Init(ins, outs)
	}
	var sumlo, sumhi int
	for i := 0; i < 1000; i++ {
		lo, hi := propagate(broadcaster)
		sumlo += lo
		sumhi += hi
	}
	fmt.Printf("low = %d, hi = %d, low x high = %d\n", sumlo, sumhi, sumlo*sumhi)
	return nil
}

func main() {
	if err := run(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
